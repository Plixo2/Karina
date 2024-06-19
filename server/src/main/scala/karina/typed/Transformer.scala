package karina.typed

import karina.files.ObjectPath
import karina.highlevel.*
import karina.highlevel.TypedArithmeticOP.*
import karina.lexer.Region
import karina.typed.FlowChecker.FlowType.{FlowReturn, FlowValue}
import karina.typed.FlowChecker.{FlowType, getFlowType}
import karina.typed.InferTransformer.TypeCheckContext
import karina.types.*
import karina.{LanguageException, LanguageTypeException}

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.*


object TypedTransformer {
    def toTypedRoot(hlPackage: HLPackage): DefaultPackage = {
        val subPath = ObjectPath(List(), allowEmpty = true)
        DefaultPackage(
            hlPackage.name,
            subPath,
            hlPackage.units.par.map(ref => toTypedUnit(subPath, ref._1, ref._2)).toList,
            hlPackage.subPackages.par.map(ref => toTypedPackage(subPath, ref)).toList
        )
    }

    private def toTypedPackage(path: ObjectPath, hlPackage: HLPackage): DefaultPackage = {
        val subPath = path.add(hlPackage.name)
        DefaultPackage(
            hlPackage.name,
            subPath,
            hlPackage.units.par.map(ref => toTypedUnit(subPath, ref._1, ref._2)).toList,
            hlPackage.subPackages.par.map(ref => toTypedPackage(subPath, ref)).toList
        )
    }

    private def toTypedUnit(path: ObjectPath, name: String, hlUnit: HLUnit): DefaultUnit = {
        val subPath = path.add(name)
        DefaultUnit(
            hlUnit.region,
            name,
            subPath,
            hlUnit.imports.par.map(ref => Import(ref.region, ref.path)).toList,
            hlUnit.classes.par.map(ref => toTypedClass(subPath, ref)).toList,
            hlUnit.functions.par.map(ref => toTypedFunction(subPath, true, ref)).toList
        )
    }

    private def toTypedClass(path: ObjectPath, hlClass: HLStruct): DefaultClass = {
        val subPath = path.add(hlClass.name)
        val definedGenerics = hlClass.genericHint
            .map(ref => ref.names.map(name => GenericType(ref.region, name, GenericSource.Class(subPath))))
            .getOrElse(List())
        val fields = hlClass.fields.map(ref => {
            Field(ref.region, ref.name, ref.tpe)
        })
        val functions = hlClass.functions.map(ref => toTypedFunction(subPath, false, ref))
        DefaultClass(hlClass.region, hlClass.name, subPath, definedGenerics, fields, functions)
    }

    private def toTypedFunction(path: ObjectPath, isStatic: Boolean, hlFunction: HLFunction): DefaultFunction = {
        val subPath = path.add(hlFunction.name)
        val returnType = hlFunction.returnType
        val parameters = hlFunction.parameters.map(ref => Parameter(ref.region, ref.name, ref.tpe, None))
        val definedGenerics = hlFunction.genericHint
            .map(ref => ref.names.map(name => GenericType(ref.region, name, GenericSource.Function(subPath))))
            .getOrElse(List())
        DefaultFunction(
            hlFunction.region,
            isStatic,
            hlFunction.name,
            subPath,
            hlFunction.modifier,
            returnType,
            parameters,
            definedGenerics,
            hlFunction.expression
        )
    }
}

object TypeResolveTransformer {
    def resolve(root: DefaultPackage): DefaultPackage = {
        DefaultPackage(
          root.packageName,
          root.path,
          root.units.map(ref => resolveUnit(root, ref)).toList,
          root.subPackages.map(ref => resolvePackage(root, ref)).toList
        )
    }

    private def resolvePackage(root: DefaultPackage, defaultPackage: DefaultPackage): DefaultPackage = {
        DefaultPackage(
          defaultPackage.packageName,
          defaultPackage.path,
          defaultPackage.units.map(ref => resolveUnit(root, ref)).toList,
          defaultPackage.subPackages.map(ref => resolvePackage(root, ref)).toList
        )
    }

    private def resolveUnit(root: DefaultPackage, unit: DefaultUnit): DefaultUnit = {
        var types = Map[String, DefaultClass]()

        unit.allImports.foreach { ref =>
            root.locateUnit(ref.path) match {
                case Some(value) => {
                    value.classes.foreach(clazz => {
                        types += (clazz.className -> clazz)
                    })
                }
                case None => {
                    root.locateClass(ref.path) match {
                        case Some(value) => {
                            types += (value.className -> value)
                        }
                        case None => {
                            if (root.locateFunction(ref.path).isEmpty) {
                                throw new LanguageException(ref.region, s"Cannot find unit or class ${ref.path}")
                            }
                        }
                    }
                }
            }
        }
        val typeLookup = TypeLookup(root, unit, types, List.empty)

        DefaultUnit(
          unit.region,
          unit.unitName,
          unit.path,
          unit.allImports,
          unit.classes.map(ref => resolveClass(root, typeLookup, ref)).toList,
          unit.allFunctions.map(ref => resolveFunction(root, typeLookup, ref)).toList
        )
    }

    private def resolveClass(root: DefaultPackage, typeLookup: TypeLookup, clazz: DefaultClass): DefaultClass = {
        val typeLookupGeneric = typeLookup.addGenericTypes(clazz.genericDefinition)
        DefaultClass(
          clazz.region,
          clazz.className,
          clazz.path,
          clazz.genericDefinition,
          clazz.fields.par
              .map(ref => {
                  Field(
                    ref.region,
                    ref.name,
                    resolveType(ref.tpe, typeLookupGeneric),
                  )
              })
              .toList,
          clazz.functions.map(ref => resolveFunction(root, typeLookupGeneric, ref))
        )
    }

    private def resolveFunction(
        root: DefaultPackage,
        typeLookup: TypeLookup,
        function: DefaultFunction
    ): DefaultFunction = {
        val typeLookupGeneric = typeLookup.addGenericTypes(function.definedGenerics)
        DefaultFunction(
          function.region,
          function.isStatic,
          function.functionName,
          function.path,
          function.modifier,
          resolveType(function.returnType, typeLookupGeneric),
          function.parameters.map(p => Parameter(p.region, p.name, resolveType(p.tpe, typeLookupGeneric), None)).toList,
          function.definedGenerics,
          resolveExpression(function.body, typeLookupGeneric)
        )
    }

    private def resolveType(tpe: Type, lookup: TypeLookup): Type = {
        tpe match {
            case ObjectTypeDefault(region, path, generics) => {
                val resolvedGenerics = generics.map(resolveType(_, lookup))
                def toClass(clazz: DefaultClass): Type = {
                    if (resolvedGenerics.isEmpty) {
                        ObjectType(region, clazz.path, Map.empty)
                    } else {

                        if (clazz.genericDefinition.length != resolvedGenerics.length) {
                            throw new NullPointerException(s"Generic count mismatch for class $path")
                        }
                        val map = clazz.genericDefinition.zip(resolvedGenerics).toMap
                        ObjectType(region, clazz.path, map)
                    }
                }

                lookup.root.locateClass(path) match {
                    case Some(clazz) => return toClass(clazz)
                    case None        => ()
                }
                lookup.unit.locateClass(path) match {
                    case Some(clazz) => return toClass(clazz)
                    case None        => ()
                }
                if (path.length != 1) {
                    throw new NullPointerException(s"Cannot resolve class $path")
                }
                val name = path.head
                lookup.importedTypes.get(name) match {
                    case Some(value) => return toClass(value)
                    case None        => ()
                }
                lookup.genericTypes.find(_.name == name) match {
                    case Some(value) => return value
                    case None        => ()
                }
                throw new LanguageException(region, s"Cannot resolve type $path")
            }
            case GenericType(region, name, source) => {
                throw new LanguageException(region, "Cannot resolve generic type")
            }
            case FunctionType(region, args, ret) => {
                FunctionType(region, args.map(resolveType(_, lookup)), resolveType(ret, lookup))
            }
            case ArrayType(region, t) => {
                ArrayType(region, resolveType(t, lookup))
            }
            case FloatType(_)     => tpe
            case VoidType(_)      => tpe
            case IntegerType(_)   => tpe
            case StringType(_)    => tpe
            case BooleanType(_)   => tpe
            case BaseType(region) => tpe
            case ObjectType(region, path, binds) => {
                throw new LanguageException(region, "Cannot resolve object type")
            }
            case r: Resolvable => {
                throw new LanguageException(r.getRegion(), "Cannot resolve Resolvable")
            }

        }
    }

    private def resolveExpression(expression: Expression, lookup: TypeLookup): Expression = {
        if (!expression.isInstanceOf[StageNode]) {
            throw new LanguageException(expression.getRegion, s"Cannot parse stage ${expression.getClass}")
        }
        expression.asInstanceOf[StageNode] match {
            case Array(region, elements) => {
                Array(region, elements.map(resolveExpression(_, lookup)))
            }
            case While(region, condition, body) => {
                While(region, resolveExpression(condition, lookup), resolveExpression(body, lookup))
            }
            case HLFor(region, variable, iterable, body) => {
                HLFor(region, variable, resolveExpression(iterable, lookup), resolveExpression(body, lookup))
            }
            case HLBlock(region, expressions) => {
                HLBlock(region, expressions.map(resolveExpression(_, lookup)))
            }
            case FloatLiteral(region, value) => {
                FloatLiteral(region, value)
            }
            case IntLiteral(region, value) => {
                IntLiteral(region, value)
            }
            case BoolLiteral(region, state) => {
                BoolLiteral(region, state)
            }
            case HLName(region, name) => {
                HLName(region, name)
            }
            case StringLiteral(region, name) => {
                StringLiteral(region, name)
            }
            case HLSelf(region) => {
                HLSelf(region)
            }
            case HLSuper(region) => {
                HLSuper(region)
            }
            case HLNew(region, tpe, initList) => {
                HLNew(
                  region,
                  resolveType(tpe, lookup),
                  initList.map(ref => {
                      HLInitMember(ref.region, ref.name, resolveExpression(ref.value, lookup))
                  })
                )
            }
            case HLBranch(region, condition, ifTrue, ifFalse) => {
                HLBranch(
                  region,
                  resolveExpression(condition, lookup),
                  resolveExpression(ifTrue, lookup),
                  ifFalse.map(resolveExpression(_, lookup))
                )
            }
            case HLBinary(region, left, right, op) => {
                HLBinary(region, resolveExpression(left, lookup), resolveExpression(right, lookup), op)
            }
            case Unary(region, value, op) => {
                Unary(region, resolveExpression(value, lookup), op)
            }
            case HLClosure(region, parameters, returnType, body) => {
                HLClosure(
                  region,
                  parameters.map(ref => {
                      HLClosureParameter(ref.region, ref.name, ref.typeHint.map(ref => resolveType(ref, lookup)))
                  }),
                  returnType.map(ref => resolveType(ref, lookup)),
                  resolveExpression(body, lookup)
                )
            }
            case Return(region, value) => {
                Return(region, value.map(resolveExpression(_, lookup)))
            }
            case Break(region) => {
                Break(region)
            }
            case HLVarDef(region, name, typeHint, value) => {
                HLVarDef(
                  region,
                  name,
                  typeHint.map(ref => resolveType(ref, lookup)),
                  resolveExpression(value, lookup)
                )
            }
            case HLDotName(region, left, name) => {
                HLDotName(region, resolveExpression(left, lookup), name)
            }
            case HLCall(region, left, expressions) => {
                HLCall(region, resolveExpression(left, lookup), expressions.map(resolveExpression(_, lookup)))
            }
            case HLArrayIndex(region, left, index) => {
                HLArrayIndex(region, resolveExpression(left, lookup), resolveExpression(index, lookup))
            }
            case Assign(region, left, right) => {
                Assign(region, resolveExpression(left, lookup), resolveExpression(right, lookup))
            }
        }
    }

    private case class TypeLookup(
        root: DefaultPackage,
        unit: DefaultUnit,
        importedTypes: Map[String, DefaultClass],
        genericTypes: List[GenericType]
    ) {
        def addGenericTypes(generics: List[GenericType]): TypeLookup = {
            TypeLookup(root, unit, importedTypes, genericTypes ++ generics)
        }
    }

}

object VariableResolveTransformer {

    def resolve(root: DefaultPackage): DefaultPackage = {
        DefaultPackage(
          root.packageName,
          root.path,
          root.units.map(ref => resolveUnit(root, ref)).toList,
          root.subPackages.map(ref => resolvePackage(root, ref)).toList
        )
    }

    private def resolvePackage(root: DefaultPackage, defaultPackage: DefaultPackage): DefaultPackage = {
        DefaultPackage(
          defaultPackage.packageName,
          defaultPackage.path,
          defaultPackage.units.map(ref => resolveUnit(root, ref)).toList,
          defaultPackage.subPackages.map(ref => resolvePackage(root, ref)).toList
        )
    }

    private def resolveUnit(root: DefaultPackage, unit: DefaultUnit): DefaultUnit = {
        var importedFunctions = Map[String, ObjectPath]()

        unit.allImports.foreach { ref =>
            root.locateUnit(ref.path) match {
                case Some(value) => {
                    value.allFunctions.foreach(fcn => {
                        importedFunctions += (fcn.functionName -> fcn.path)
                    })
                }
                case None => {
                    root.locateFunction(ref.path) match {
                        case Some(value) => {
                            if (!value.isStatic) {
                                throw new LanguageException(
                                  ref.region,
                                  s"Cannot import non static function ${ref.path}"
                                )
                            }
                            importedFunctions += (value.functionName -> value.path)
                        }
                        case None => {
                            if (root.locateUnit(ref.path).isEmpty) {
                                throw new LanguageException(ref.region, s"Cannot find unit or function ${ref.path}")
                            }
                        }
                    }
                }
            }
        }
        unit.allFunctions.foreach { ref =>
            importedFunctions += (ref.functionName -> ref.path)
        }

        DefaultUnit(
          unit.region,
          unit.unitName,
          unit.path,
          unit.allImports,
          unit.classes.map(ref => resolveClass(root, ref, importedFunctions)).toList,
          unit.allFunctions.map(ref => resolveFunction(root, ref, importedFunctions)).toList
        )
    }

    private def resolveClass(
        root: DefaultPackage,
        clazz: DefaultClass,
        importedFunctions: Map[String, ObjectPath]
    ): DefaultClass = {

        val emptyContext = ResolvingContext(root, List.empty, importedFunctions, VariableUsageContainer(Map.empty))
        DefaultClass(
          clazz.region,
          clazz.className,
          clazz.path,
          clazz.genericDefinition,
          clazz.fields.map(ref =>
              Field(ref.region, ref.name, ref.tpe)
          ),
          clazz.functions.map(ref => resolveFunction(root, ref, importedFunctions)),
        )
    }

    private def resolveFunction(
        root: DefaultPackage,
        function: DefaultFunction,
        importedFunctions: Map[String, ObjectPath]
    ): DefaultFunction = {

        var parameters = List[Parameter]()
        val mutableVarUseCounter = VariableUsageContainer(Map.empty)
        val context =
            function.parameters.foldLeft(ResolvingContext(root, List.empty, importedFunctions, mutableVarUseCounter))(
              (context, parameter) => {
                  if (context.doesVariableExist(parameter.name)) {
                      throw new LanguageException(parameter.region, s"Variable ${parameter.name} already exists")
                  }
                  val variable = Variable(parameter.region, parameter.name, VariableLocation.Function, false)
                  parameters = parameters :+ Parameter(parameter.region, parameter.name, parameter.tpe, Some(variable))
                  context.addVariable(variable)
              }
            )

        DefaultFunction(
          function.region,
          function.isStatic,
          function.functionName,
          function.path,
          function.modifier,
          function.returnType,
          parameters,
          function.definedGenerics,
          resolveExpression(function.body, context)._1
        )
    }

    private def resolveExpression(expression: Expression, context: ResolvingContext): (Expression, ResolvingContext) = {
        if (!expression.isInstanceOf[StageNode]) {
            throw new LanguageException(expression.getRegion, s"Cannot parse stage ${expression.getClass}")
        }
        expression.asInstanceOf[StageNode] match {
            case Array(region, elements) => {
                val supElements = elements.map(ref => resolveExpression(ref, context)._1)
                (Array(region, supElements), context)
            }
            case While(region, condition, body) => {
                val (supCondition, _) = resolveExpression(condition, context)
                val (supBody, _) = resolveExpression(body, context)
                (While(region, supCondition, supBody), context)
            }
            case HLFor(region, variable, iterable, body) => {
                if (context.doesVariableExist(variable)) {
                    throw new LanguageException(region, s"Variable ${variable} already exists")
                }
                val elementVariable = Variable(region, variable, VariableLocation.Local, false)
                val bodyContext = context.addVariable(elementVariable)
                (
                  For(
                    region,
                    elementVariable,
                    resolveExpression(iterable, context)._1,
                    resolveExpression(body, bodyContext)._1
                  ),
                  context
                )
            }
            case HLBlock(region, expressions) => {
                var newContext = context
                val newExpressions = expressions.map(ref => {
                    val (expr, nextContext) = resolveExpression(ref, newContext)
                    newContext = nextContext
                    expr
                })
                (HLBlock(region, newExpressions), context)
            }
            case FloatLiteral(region, value) => (expression, context)
            case IntLiteral(region, value)   => (expression, context)
            case BoolLiteral(region, state)  => (expression, context)
            case HLName(region, name) => {
                if (context.doesVariableExist(name)) {
                    val variable = context.getVariable(name).get
                    context.varUsage.useVariable(variable)
                    (VariableLink(region, variable), context)
                } else if (context.doesFunctionExist(name)) {
                    (StaticFunction(region, context.getFunction(name).get), context)
                } else {
                    val path = ObjectPath(List(name), false)
                    context.root.locateUnit(path) match {
                        case Some(value) => {
                            UnitPath(region, value.path)
                        }
                        case None => {
                            context.root.locatePackage(path) match {
                                case Some(value) => {
                                    PackagePath(region, value.path)
                                }
                                case None => {
                                    throw new LanguageException(region, s"Unknown Variable, Path or Function '${name}'")
                                }
                            }
                        }
                    }
                    (expression, context)
                }
            }
            case StringLiteral(region, name) => (expression, context)
            case HLSelf(region)              => (expression, context)
            case HLSuper(region)             => (expression, context)
            case HLNew(region, tpe, initList) => {
                val hLNew = HLNew(
                  region,
                  tpe,
                  initList.map(ref => HLInitMember(ref.region, ref.name, resolveExpression(ref.value, context)._1))
                )
                (hLNew, context)
            }
            case HLBranch(region, condition, ifTrue, ifFalse) => {
                val (supCondition, _) = resolveExpression(condition, context)
                val supIfFalse = ifFalse.map(ref => resolveExpression(ref, context)._1)
                var subContext = context
                val (supIfTrue, _) = resolveExpression(ifTrue, subContext)
                (Branch(region, supCondition, supIfTrue, supIfFalse), context)
            }
            case HLBinary(region, left, right, op) => {
                val (supLeft, _) = resolveExpression(left, context)
                val (supRight, _) = resolveExpression(right, context)
                (HLBinary(region, supLeft, supRight, op), context)
            }
            case Unary(region, value, op) => {
                val (supValue, _) = resolveExpression(value, context)
                (Unary(region, supValue, op), context)
            }
            case HLClosure(region, parameters, returnType, body) => {
                var bodyContext = context
                val preUsedVariables = context.varUsage.usedVariables
                val closureParameters = parameters.map(ref => {
                    if (bodyContext.doesVariableExist(ref.name)) {
                        throw new LanguageException(region, s"Variable ${ref.name} already exists")
                    }
                    val variable = Variable(ref.region, ref.name, VariableLocation.Closure, false)
                    bodyContext = bodyContext.addVariable(variable)
                    ClosureParameter(region, variable, ref.typeHint)
                })
                val (supBody, usedVariableInfoContext) = resolveExpression(body, bodyContext)
                var usedVariablesOfClosure = List[Variable]()
                preUsedVariables.foreach((variable, count) => {
                    val newUsageCount: Integer = usedVariableInfoContext.varUsage.usedVariables.getOrElse(variable, 0)
                    if (newUsageCount > count) {
                        usedVariablesOfClosure = usedVariablesOfClosure :+ variable
                        println(s"Variable ${variable.name} is used in closure")
                    }
                })

                (Closure(region, closureParameters, returnType, supBody, usedVariablesOfClosure), context)
            }
            case Return(region, value) => {
                val supValue = value.map(ref => resolveExpression(ref, context)._1)
                (Return(region, supValue), context)
            }
            case Break(region) => {
                (expression, context)
            }
            case HLVarDef(region, name, typeHint, value) => {
                if (context.doesVariableExist(name)) {
                    throw new LanguageException(region, s"Variable ${name} already exists")
                }
                val variable = Variable(region, name, VariableLocation.Local, true)
                val newContext = context.addVariable(variable)
                val (newValue, _) = resolveExpression(value, context)
                (VarDef(region, variable, typeHint, newValue), newContext)
            }
            case HLDotName(region, left, name) => {
                val leftExpr = resolveExpression(left, context)._1
                def locateNext(path: ObjectPath): Expression = {
                    context.root.locateUnit(path) match {
                        case Some(value) => {
                            UnitPath(region, value.path)
                        }
                        case None => {
                            context.root.locatePackage(path) match {
                                case Some(value) => {
                                    PackagePath(region, value.path)
                                }
                                case None => {
                                    context.root.locateFunction(path) match {
                                        case Some(value) => {
                                            StaticFunction(region, value.path)
                                        }
                                        case None => {
                                            throw new LanguageException(region, s"Unknown Path or Function '${name}'")
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                leftExpr match {
                    case PackagePath(region, path) => {
                        (locateNext(path.add(name)), context)
                    }
                    case UnitPath(region, path) => {
                        (locateNext(path.add(name)), context)
                    }
                    case _ => {
                        (HLDotName(region, leftExpr, name), context)
                    }
                }
            }
            case HLCall(region, left, expressions) =>
                (
                  HLCall(
                    region,
                    resolveExpression(left, context)._1,
                    expressions.map(ref => resolveExpression(ref, context)._1)
                  ),
                  context
                )
            case HLArrayIndex(region, left, index) =>
                (
                  HLArrayIndex(region, resolveExpression(left, context)._1, resolveExpression(index, context)._1),
                  context
                )
            case Assign(region, left, right) =>
                (
                  Assign(region, resolveExpression(left, context)._1, resolveExpression(right, context)._1),
                  context
                )
        }
    }

    private case class ResolvingContext(
        root: DefaultPackage,
        variables: List[Variable],
        functions: Map[String, ObjectPath],
        varUsage: VariableUsageContainer
    ) {
        def addVariable(variable: Variable): ResolvingContext = {
            varUsage.useVariable(variable)
            ResolvingContext(root, variables :+ variable, functions, varUsage)
        }

        def getVariable(name: String): Option[Variable] = {
            variables.find(_.name == name)
        }

        def doesVariableExist(name: String): Boolean = {
            getVariable(name).isDefined
        }

        def doesFunctionExist(name: String): Boolean = {
            getFunction(name).isDefined
        }
        def getFunction(name: String): Option[ObjectPath] = {
            functions.get(name)
        }
    }

    private class VariableUsageContainer(var usedVariables: Map[Variable, Integer]) {
        def useVariable(variable: Variable): Unit = {
            val index: Integer = usedVariables.getOrElse(variable, 0)
            usedVariables = usedVariables + (variable -> (index + 1))
        }
    }
}

object InferTransformer {

    def resolve(root: DefaultPackage): DefaultPackage = {
        DefaultPackage(
          root.packageName,
          root.path,
          root.units.map(ref => resolveUnit(root, ref)).toList,
          root.subPackages.map(ref => resolvePackage(root, ref)).toList
        )
    }

    private def resolvePackage(root: DefaultPackage, defaultPackage: DefaultPackage): DefaultPackage = {
        DefaultPackage(
          defaultPackage.packageName,
          defaultPackage.path,
          defaultPackage.units.map(ref => resolveUnit(root, ref)).toList,
          defaultPackage.subPackages.map(ref => resolvePackage(root, ref)).toList
        )
    }

    private def resolveUnit(root: DefaultPackage, unit: DefaultUnit): DefaultUnit = {

        DefaultUnit(
          unit.region,
          unit.unitName,
          unit.path,
          unit.allImports,
          unit.classes.map(ref => resolveClass(root, ref)),
          unit.allFunctions.map(ref => resolveFunction(root, ref, None))
        )
    }

    private def resolveClass(
        root: DefaultPackage,
        clazz: DefaultClass
    ): DefaultClass = {
        val selfType = ObjectType(clazz.region, clazz.path, Map.empty)

        DefaultClass(
          clazz.region,
          clazz.className,
          clazz.path,
          clazz.genericDefinition,
          clazz.fields.map(ref =>
              Field(
                ref.region,
                ref.name,
                ref.tpe
              )
          ),
          clazz.functions.map(ref => resolveFunction(root, ref, Some(selfType))),
        )
    }

    private def resolveFunction(
        root: DefaultPackage,
        function: DefaultFunction,
        selfType: Option[Type]
    ): DefaultFunction = {
        if (function.modifier == Modifier.Native) {
            return function;
        }
        val context = TypeCheckContext(root, selfType, function.returnType, Map.empty)

        function.parameters.foreach(ref => {
            context.addVariableType(
              ref.variableObject.expectWithRegion(ref.region, "variable object on function parameter "),
              ref.tpe
            )
        })

        val expression = resolveExpression(Some(function.returnType), context, function.body)
        val returningExpression = getFlowType(expression) match {
            case FlowReturn(tpe) => expression
            case FlowValue(tpe) => {
                if (!Checker.isAssignable(context, function.returnType, tpe, true)) {
                    throw new LanguageTypeException(expression.getRegion, function.returnType, tpe)
                }
                Return(expression.getRegion, Some(expression))
            }
        }

        DefaultFunction(
          function.region,
          function.isStatic,
          function.functionName,
          function.path,
          function.modifier,
          function.returnType,
          function.parameters,
          function.definedGenerics,
          returningExpression
        )
    }

    private def resolveExpression(hint: Option[Type], context: TypeCheckContext, expression: Expression): Expression = {
        if (!expression.isInstanceOf[StageVariable]) {
            throw new LanguageException(expression.getRegion, s"Cannot parse stage ${expression.getClass}")
        }

        hint match {
            case Some(value) => {
                value match {
                    case resolvable: Resolvable => {
                        if (resolvable.isResolved) {
                            return resolveExpression(Some(resolvable.get()), context, expression)
                        }
                    }
                    case _ => {}
                }
            }
            case None => {}
        }
        expression.asInstanceOf[StageVariable] match {
            case Array(region, elements) => {
                def calcSuperTypeAndDependTypes(): TypedArray = {
                    var allExpressions = List[Expression]()
                    val firstExpression = resolveExpression(None, context, elements.head)
                    val firstType = ExpressionType.getType(firstExpression)
                    allExpressions = allExpressions :+ firstExpression
                    elements.tail.foldLeft(firstType)((tpe, ref) => {
                        val expression = resolveExpression(Some(tpe), context, ref)
                        allExpressions = allExpressions :+ expression
                        val expressionType = ExpressionType.getType(expression)
                        calculateSuperType(context, region, tpe, expressionType)
                    })
                    val types = allExpressions.map(ref => ExpressionType.getType(ref))
                    val superType = checkSuperType(context, region, context.root, types)
                    TypedArray(region, superType, allExpressions)
                }

                hint match {
                    case Some(value) => {
                        value match {
                            case ArrayType(region, t) => {
                                if (elements.isEmpty) {
                                    TypedArray(region, t, List())
                                } else {
                                    val expressions = elements.map(ref => resolveExpression(Some(t), context, ref))
                                    val types = expressions.map(ref => ExpressionType.getType(ref))
                                    val superType = checkSuperType(context, region, context.root, types)
                                    TypedArray(region, superType, expressions)
                                }
                            }
                            case _ => {
                                if (elements.isEmpty) {
                                    TypedArray(region, Resolvable(region), List())
                                } else {
                                    calcSuperTypeAndDependTypes()
                                }
                            }
                        }
                    }
                    case None => {
                        if (elements.isEmpty) {
                            TypedArray(region, Resolvable(region), List())
                        } else {
                            calcSuperTypeAndDependTypes()
                        }
                    }
                }
            }
            case While(region, condition, body) => {
                val conditionChecked = resolveExpression(Some(BooleanType(region)), context, condition)
                While(region, conditionChecked, resolveExpression(None, context, body))
            }
            case HLBlock(region, expressions) => {
                if (expressions.isEmpty) {
                    TypedBlock(region, List(), VoidType(region))
                } else {
                    val last = expressions.last
                    val checked = expressions.map(ref => {
                        if (ref == last) {
                            resolveExpression(hint, context, ref)
                        } else {
                            val expr = resolveExpression(None, context, ref)
                            val flowType = FlowChecker.getFlowType(expr)
                            flowType match {
                                case FlowType.FlowReturn(tpe) => {
                                    throw new LanguageException(ref.getRegion, "dead code")
                                }
                                case FlowType.FlowValue(tpe) => {}
                            }
                            expr
                        }
                    })
                    TypedBlock(region, checked, ExpressionType.getType(checked.last))
                }
            }
            case FloatLiteral(region, value) => {
                hint.flatMap(ref =>
                    ref match {
                        case fcn: IntegerType => Some(fcn)
                        case _                => None
                    }
                ) match {
                    case Some(_) => {
                        if (value.toLong.toDouble == value.doubleValue) {
                            IntLiteral(region, value.toLong)
                        } else {
                            FloatLiteral(region, value)
                        }
                    }
                    case None => {
                        FloatLiteral(region, value)
                    }
                }

            }
            case IntLiteral(region, value) => {
                hint.flatMap(ref =>
                    ref match {
                        case fcn: FloatType => Some(fcn)
                        case _              => None
                    }
                ) match {
                    case Some(_) => {
                        FloatLiteral(region, value.doubleValue)
                    }
                    case None => {
                        IntLiteral(region, value)
                    }
                }
            }
            case BoolLiteral(region, state) => {
                BoolLiteral(region, state)
            }
            case StringLiteral(region, name) => {
                StringLiteral(region, name)
            }
            case HLSelf(region) => {
                context.selfContext match {
                    case Some(value) => {
                        TypedSelf(region, value)
                    }
                    case None => {
                        throw new LanguageException(region, "Cannot use 'self' outside of class")
                    }
                }
            }
            case HLSuper(region) => {
                throw new LanguageException(region, "Cannot use 'super' for now")
            }
            case HLNew(region, tpe, initList) => {
                tpe match {
                    case ObjectType(region, path, binds) => {
                        val clazz = context.root.locateClass(path).expectWithRegion(region, "unit on path")
                        var bindings = Map[GenericType, Type]()
                        clazz.genericDefinition.foreach(ref => {
                            if (binds.contains(ref)) {
                                bindings = bindings + (ref -> binds(ref))
                            } else {
                                bindings = bindings + (ref -> Resolvable(ref.region))
                            }
                        })
                        val boundObject = ObjectType(region, path, bindings)
                        if (clazz.fields.length != initList.length) {
                            throw new LanguageException(region, "Parameter count mismatch")
                        }
                        var sortedParameterExpressions = List[Expression]()
                        var unInitFields = clazz.fields
                        initList.foreach(ref => {
                            unInitFields.find(param => ref.name == param.name) match {
                                case Some(value) => {
                                    val expectedType = boundObject.getTypeReplaced(value.tpe)
                                    val expressionValue = resolveExpression(Some(expectedType), context, ref.value)
                                    val typeOfExpression = ExpressionType.getType(expressionValue)
                                    if (!Checker.isAssignable(context, expectedType, typeOfExpression, true)) {
                                        throw new LanguageTypeException(region, expectedType, typeOfExpression)
                                    }
                                    sortedParameterExpressions = sortedParameterExpressions :+ expressionValue
                                }
                                case None => {
                                    throw new LanguageException(region, "Parameter not found")
                                }
                            }
                            unInitFields = unInitFields.filterNot(param => ref.name == param.name)
                        })
                        assert(unInitFields.isEmpty, "Not all fields are initialized")
                        TypedNew(region, boundObject, sortedParameterExpressions)
                    }
                    case _ => {
                        throw new LanguageException(region, s"Cannot only create objects with 'new' ${tpe.getClass}")
                    }
                }
            }
            case b: Branch => checkBranch(hint, context, b)
            case HLBinary(region, left, right, op) => {
                checkBinary(region, hint, context, left, right, op)
            }
            case Unary(region, value, op) => {
                op match {
                    case UnaryOP.Negate => {
                        val expr = resolveExpression(Some(hint.getOrElse(FloatType(region))), context, value)
                        val exprType = ExpressionType.getType(expr)
                        if (ExpressionType.isInt(exprType, context)) {
                            TypedUnary(region, expr, TypedUnaryArithmeticOP.NEGATE_INT)
                        } else if (ExpressionType.isFloat(exprType, context)) {
                            TypedUnary(region, expr, TypedUnaryArithmeticOP.NEGATE_FLOAT)
                        } else {
                            throw new LanguageException(region, "Cannot negate non numeric types")
                        }
                    }
                    case UnaryOP.Not => {
                        val expr = resolveExpression(Some(BooleanType(region)), context, value)
                        val exprType = ExpressionType.getType(expr)
                        if (ExpressionType.isBool(exprType, context)) {
                            TypedUnary(region, expr, TypedUnaryArithmeticOP.NOT)
                        } else {
                            throw new LanguageTypeException(region, BooleanType(region), exprType)
                        }
                    }
                }
            }
            case Closure(region, parameters, returnType, body, _) => {
                def resolveClosure(returnDefault: Type, paramDefaults: List[Type]): Expression = {
                    assert(paramDefaults.length == parameters.length, "Parameter count mismatch")
                    val parameterTypes = parameters
                        .zip(paramDefaults)
                        .map((ref, hint) => {
                            val typeOfVariable = ref.typeHint.getOrElse(hint)
                            context.addVariableType(ref.variable, typeOfVariable)
                            (ref.variable, typeOfVariable)
                        })
                    val returnTyped = returnType.getOrElse(returnDefault)
                    val childContext = context.withReturnType(returnTyped)

                    val expression = resolveExpression(Some(returnTyped), childContext, body)
                    val typeOfExpr = ExpressionType.getType(expression)
                    Checker.isAssignable(childContext, returnTyped, typeOfExpr, true)
//                    if (!Checker.isAssignable(childContext, returnTyped, typeOfExpr, true)) {
//                        throw new LanguageTypeException(region, returnTyped, typeOfExpr)
//                    }
                    TypedClosure(region, parameterTypes, returnTyped, expression)
                }
                hint.flatMap(ref =>
                    ref match {
                        case fcn: FunctionType => Some(fcn)
                        case _                 => None
                    }
                ) match {
                    case Some(value) => {
                        val defaultParams = if (value.args.length == parameters.length) {
                            value.args
                        } else {
                            List.range(0, parameters.length).map(_ => Resolvable(region))
                        }
                        resolveClosure(value.ret, defaultParams)
                    }
                    case None => {
                        resolveClosure(
                          Resolvable(region),
                          List.range(0, parameters.length).map(_ => Resolvable(region))
                        )
                    }
                }
            }
            case Return(region, value) => {
                val valueChecked = value.map(ref => {
                    val expr = resolveExpression(Some(context.returnType), context, ref)
                    expr
                })
                val returningType =
                    valueChecked
                        .map(ref => {
                            val value1 = ExpressionType.getType(ref)
                            value1
                        })
                        .getOrElse(VoidType(region))

                if (!Checker.isAssignable(context, context.returnType, returningType, true)) {
                    throw new LanguageTypeException(region, context.returnType, returningType)
                }

                Return(region, valueChecked)
            }
            case Break(region) => {
//                Break(region)
                throw new LanguageException(region, "Break is not implemented")
            }
            case HLDotName(region, left, name) => {
                val leftChecked = resolveExpression(None, context, left)
                val leftType = ExpressionType.getType(leftChecked)
                leftType match {
                    case r: ObjectType => {
                        r.getField(context.root, name) match {
                            case Some(value) => {
                                TypedField(region, r, leftChecked, value, name)
                            }
                            case None => {
                                r.getFunction(context.root, name) match {
                                    case Some((tpe, path)) => {
                                        val expr = TypedFunction(region, r, leftChecked, name, path, tpe.args, tpe.ret)
                                        expr
                                    }
                                    case None => {
                                        throw new LanguageException(region, s"Field $name not found")
                                    }
                                }
                            }
                        }
                    }
                    case _ => {
                        throw new LanguageException(region, "Cannot access fields on non objects")
                    }
                }
            }
            case HLCall(region, left, expressions) => {
                val callValue = resolveExpression(None, context, left)
                callValue match {
                    case TypedStaticFunction(region, path, inputTypes, returnType) => {
                        val staticFunction =
                            context.root.locateFunction(path).expectWithRegion(region, "get static function")
                        if (!staticFunction.isStatic) {
                            throw new LanguageException(region, "Function is not static")
                        }
                        if (staticFunction.parameters.length != expressions.length) {
                            throw new LanguageException(region, "Parameter count mismatch")
                        }
                        val expressionsList = inputTypes
                            .zip(expressions)
                            .map(ref => {
                                val expr = resolveExpression(Some(ref._1), context, ref._2)
                                val exprValue = ExpressionType.getType(expr)
                                if (!Checker.isAssignable(context, ref._1, exprValue, true)) {
                                    throw new LanguageTypeException(region, ref._1, exprValue)
                                }
                                expr
                            })
                        CallStaticFunction(region, path, returnType, expressionsList)
                    }
                    case TypedFunction(region, objType, obj, name, path, args, ret) => {
                        if (args.length != expressions.length) {
                            throw new LanguageException(region, "Parameter count mismatch")
                        }
                        val expressionsList = args
                            .zip(expressions)
                            .map(ref => {
                                val expr = resolveExpression(Some(ref._1), context, ref._2)
                                val exprValue = ExpressionType.getType(expr)
                                if (!Checker.isAssignable(context, ref._1, exprValue, true)) {
                                    throw new LanguageTypeException(region, ref._1, exprValue)
                                }
                                expr
                            })
                        CallDynamicFunctionDirect(region, obj, path, ret, expressions)
                    }
                    case _ => {
                        val callType = ExpressionType.getType(callValue)
                        callType match {
                            case FunctionType(region, args, ret) => {
                                if (args.length != expressions.length) {
                                    throw new LanguageException(region, "Parameter count mismatch")
                                }
                                val expressionsList = args
                                    .zip(expressions)
                                    .map(ref => {
                                        val expr = resolveExpression(Some(ref._1), context, ref._2)
                                        val exprValue = ExpressionType.getType(expr)
                                        if (!Checker.isAssignable(context, ref._1, exprValue, true)) {
                                            throw new LanguageTypeException(region, ref._1, exprValue)
                                        }
                                        expr
                                    })

                                CallDynamicFunctionIndirect(region, callValue, ret, expressionsList)
                            }
                            case resolvable: Resolvable => {
                                assert(!resolvable.isResolved, "Function type is resolved")
                                val newExpr = expressions.map(ref => resolveExpression(None, context, ref))
                                val exprTypes = newExpr.map(ref => ExpressionType.getType(ref))
                                val returnTypeDef = hint.getOrElse(Resolvable(region))
                                resolvable.resolve(FunctionType(region, exprTypes, returnTypeDef))
                                CallDynamicFunctionIndirect(region, callValue, returnTypeDef, newExpr)
                            }
                            case s => {
                                throw new LanguageException(region, s"Cannot call ${s.getClass}")
                            }
                        }
                    }
                }
            }
            case HLArrayIndex(region, left, index) => {
                val indexChecked = resolveExpression(Some(IntegerType(region)), context, index)
                val leftValue = resolveExpression(hint.map(ArrayType(region, _)), context, left)
                HLArrayIndex(region, leftValue, indexChecked)
            }
            case For(region, variable, iterable, body) => {
                val iterableChecked = resolveExpression(Some(ArrayType(region, Resolvable(region))), context, iterable)
                val testedIterable = ExpressionType.getType(iterableChecked)

                testedIterable match {
                    case ArrayType(region, t) => {
                        context.addVariableType(variable, t)
                        val bdy = resolveExpression(None, context, body)
                        TypedFor(region, variable, t, iterableChecked, bdy)
                    }
                    case _ => {
                        throw new LanguageException(region, "Cannot iterate over non array types")
                    }
                }
            }
            case StaticFunction(region, path) => {
                val functions = context.root.locateFunction(path).expectWithRegion(region, "function from path")
                val binds = functions.definedGenerics.map(ref => (ref, Resolvable(ref.region))).toMap
                val inputTypes = functions.parameters.map(ref => ref.tpe)
                val boundInputs = inputTypes.map(ref => getReplacedType(ref, binds))
                val returnType = getReplacedType(functions.returnType, binds)
                TypedStaticFunction(region, path, boundInputs, returnType)
            }
            case VariableLink(region, variable) => {
                val varType = context.getVariableType(variable)
                TypedVariableLink(region, variable, varType)
            }
            case VarDef(region, variable, typeHint, value) => {
                val valueExpression = resolveExpression(typeHint, context, value)
                typeHint match {
                    case Some(value) => {
                        val valueType = ExpressionType.getType(valueExpression)
                        if (!Checker.isAssignable(context, value, valueType, true)) {
                            throw new LanguageTypeException(region, value, valueType)
                        }
                        context.addVariableType(variable, value)
                        println(s"Type ${valueType} as ${value}")
                        VarDef(region, variable, typeHint, valueExpression)
                    }
                    case None => {
                        val valueType = ExpressionType.getType(valueExpression)
                        context.addVariableType(variable, valueType)
                        println(s"Type ${valueType}")
                        VarDef(region, variable, Some(valueType), valueExpression)
                    }
                }

            }
            case PackagePath(region, path) => {
                throw new LanguageException(region, "Cannot check package path")
            }
            case UnitPath(region, path) => {
                throw new LanguageException(region, "Cannot check unit path")
            }
            case Assign(region, left, right) => {
                val leftChecked = resolveExpression(None, context, left)
                val leftType = ExpressionType.getType(leftChecked)
                val rightChecked = resolveExpression(Some(leftType), context, right)
                val rightType = ExpressionType.getType(rightChecked)
                if (!Checker.isAssignable(context, leftType, rightType, true)) {
                    throw new LanguageTypeException(region, leftType, rightType)
                }
                leftChecked match {
                    case TypedVariableLink(region, variable, tpe) => {
                        if (!variable.isMutable) {
                            throw new LanguageException(region, "Cannot assign to immutable variable")
                        }
                        TypedAssignVariable(region, variable, context.getVariableType(variable), rightChecked)
                    }
                    case HLArrayIndex(region, left, index) => {
                        TypedAssignArray(region, left, index, rightChecked)
                    }
                    case TypedField(region, objType, obj, fieldType, field) => {
                        TypedAssignField(region, objType, obj, fieldType, field, rightChecked)
                    }
                    case _ => {
                        throw new LanguageException(region, "Cannot assign")
                    }
                }
            }
        }
    }

    private def checkBranch(hint: Option[Type], context: TypeCheckContext, branch: Branch): Expression = branch match {
        case Branch(region, condition, ifTrue, ifFalse) => {
            def resolveBody(conditionChecked: Expression): TypedBranch = {
                var typeHint = hint;
                if (ifFalse.isEmpty) {
                    typeHint = None
                }
                val ifTrueChecked = resolveExpression(
                  typeHint,
                  context,
                  ifTrue
                )
                val firstCase = FlowChecker.getFlowType(ifTrueChecked)
                firstCase match {
                    case FlowType.FlowValue(tpe) => {
                        if (typeHint.isEmpty) {
                            typeHint = Some(tpe)
                        }
                    }
                    case FlowType.FlowReturn(_) => {}
                }
                ifFalse match {
                    case Some(value) => {
                        val ifFalseChecked = resolveExpression(
                          typeHint,
                          context,
                          value
                        )
                        val secondCase = FlowChecker.getFlowType(ifFalseChecked)
                        val yieldValue = secondCase match {
                            case FlowType.FlowValue(secondTpe) => {
                                firstCase match {
                                    case FlowType.FlowValue(firstTpe) => {
                                        calculateSuperTypeOpt(context, region, firstTpe, secondTpe).getOrElse(
                                          VoidType(region)
                                        )
                                    }
                                    case FlowType.FlowReturn(_) => {
                                        secondTpe
                                    }
                                }
                            }
                            case FlowType.FlowReturn(_) => {
                                firstCase match {
                                    case FlowType.FlowValue(firstTpe) => {
                                        firstTpe
                                    }
                                    case FlowType.FlowReturn(_) => {
                                        VoidType(region)
                                    }
                                }
                            }
                        }
                        TypedBranch(region, conditionChecked, ifTrueChecked, Some(yieldValue, ifFalseChecked))
                    }
                    case None => {
                        TypedBranch(region, conditionChecked, ifTrueChecked, None)
                    }
                }
            }
            val conditionChecked = resolveExpression(Some(BooleanType(region)), context, condition)
            val typeOfCondition = ExpressionType.getType(conditionChecked)
            if (!Checker.isAssignable(context, BooleanType(region), typeOfCondition, true)) {
                throw new LanguageException(region, "Condition must be boolean")
            }
            resolveBody(conditionChecked)

        }
    }

    private def checkBinary(
        region: Region,
        hint: Option[Type],
        context: TypeCheckContext,
        left: Expression,
        right: Expression,
        op: BinaryOP
    ): Expression = {
        val hinting = op match {
            case BinaryOP.Concat             => hint
            case BinaryOP.Add                => hint
            case BinaryOP.Subtract           => hint
            case BinaryOP.Multiply           => hint
            case BinaryOP.Divide             => hint
            case BinaryOP.Modulo             => hint
            case BinaryOP.Equal              => None
            case BinaryOP.NotEqual           => None
            case BinaryOP.GreaterThan        => None
            case BinaryOP.LessThan           => None
            case BinaryOP.GreaterThanOrEqual => None
            case BinaryOP.LessThanOrEqual    => None
            case BinaryOP.And                => Some(BooleanType(region))
            case BinaryOP.Or                 => Some(BooleanType(region))
        }

        val leftChecked = resolveExpression(hinting, context, left)

        def boolTypes(opInt: TypedArithmeticOP, opName: String): Expression = {
            val leftType = ExpressionType.getType(leftChecked)
            val rightChecked = resolveExpression(Some(BooleanType(region)), context, right)
            val rightType = ExpressionType.getType(rightChecked)
            if (ExpressionType.isBool(leftType, context)) {
                if (ExpressionType.isBool(rightType, context)) {
                    TypedBinary(region, leftChecked, rightChecked, opInt)
                } else {
                    throw new LanguageException(region, s"Cannot convert ${opName} with 'bool' and a non bool")
                }
            } else {
                throw new LanguageException(region, s"Cannot convert ${opName} with non bool types")
            }
        }

        def boolOrNumberTypes(
            opInt: TypedArithmeticOP,
            opFloat: TypedArithmeticOP,
            opBool: TypedArithmeticOP,
            opName: String
        ): Expression = {
            val leftType = ExpressionType.getType(leftChecked)
            val rightChecked = resolveExpression(Some(hinting.getOrElse(leftType)), context, right)
            val rightType = ExpressionType.getType(rightChecked)
            if (ExpressionType.isInt(leftType, context)) {
                if (ExpressionType.isInt(rightType, context)) {
                    TypedBinary(region, leftChecked, rightChecked, opInt)
                } else if (ExpressionType.isFloat(rightType, context)) {
                    TypedBinary(region, IntToFloat(region, leftChecked), rightChecked, opFloat)
                } else {
                    throw new LanguageException(region, s"Cannot convert ${opName} with 'int' and a non numeric types")
                }
            } else if (ExpressionType.isFloat(leftType, context)) {
                if (ExpressionType.isInt(rightType, context)) {
                    TypedBinary(region, leftChecked, IntToFloat(region, rightChecked), opFloat)
                } else if (ExpressionType.isFloat(rightType, context)) {
                    TypedBinary(region, leftChecked, rightChecked, opFloat)
                } else {
                    throw new LanguageException(
                      region,
                      s"Cannot convert ${opName} with 'float' and a non numeric types"
                    )
                }
            } else if (ExpressionType.isBool(leftType, context)) {
                if (ExpressionType.isBool(rightType, context)) {
                    TypedBinary(region, leftChecked, rightChecked, opBool)
                } else {
                    throw new LanguageException(region, s"Cannot convert ${opName} with 'bool' and a non bool")
                }
            } else {
                throw new LanguageException(region, s"Cannot convert ${opName} with non numeric types")
            }
        }

        def intFloatTypes(opInt: TypedArithmeticOP, opFloat: TypedArithmeticOP, opName: String): Expression = {
            val leftType = ExpressionType.getType(leftChecked)
            val rightChecked = resolveExpression(Some(hinting.getOrElse(leftType)), context, right)
            val rightType = ExpressionType.getType(rightChecked)
            if (ExpressionType.isInt(leftType, context)) {
                if (ExpressionType.isInt(rightType, context)) {
                    TypedBinary(region, leftChecked, rightChecked, opInt)
                } else if (ExpressionType.isFloat(rightType, context)) {
                    TypedBinary(region, IntToFloat(region, leftChecked), rightChecked, opFloat)
                } else {
                    throw new LanguageException(region, s"Cannot convert ${opName} with 'int' and a non numeric types")
                }
            } else if (ExpressionType.isFloat(leftType, context)) {
                if (ExpressionType.isInt(rightType, context)) {
                    TypedBinary(region, leftChecked, IntToFloat(region, rightChecked), opFloat)
                } else if (ExpressionType.isFloat(rightType, context)) {
                    TypedBinary(region, leftChecked, rightChecked, opFloat)
                } else {
                    throw new LanguageException(
                      region,
                      s"Cannot convert ${opName} with 'float' and a non numeric types"
                    )
                }
            } else {
                throw new LanguageException(region, s"Cannot convert ${opName} with non numeric types")
            }
        }

        op match {
            case BinaryOP.Add => {
                intFloatTypes(IntAdd, TypedArithmeticOP.FloatAdd, "add")
            }
            case BinaryOP.Subtract => {
                intFloatTypes(IntSubtract, TypedArithmeticOP.FloatSubtract, "subtract")
            }
            case BinaryOP.Multiply => {
                intFloatTypes(IntMultiply, TypedArithmeticOP.FloatMultiply, "multiply")
            }
            case BinaryOP.Divide => {
                intFloatTypes(IntDivide, TypedArithmeticOP.FloatDivide, "divide")
            }
            case BinaryOP.Modulo => {
                val leftType = ExpressionType.getType(leftChecked)
                val rightChecked = resolveExpression(Some(IntegerType(region)), context, right)
                val rightType = ExpressionType.getType(rightChecked)
                if (!ExpressionType.isInt(leftType, context)) {
                    throw new LanguageTypeException(region, leftType, IntegerType(region))
                } else if (!ExpressionType.isInt(rightType, context)) {
                    throw new LanguageTypeException(region, rightType, IntegerType(region))
                } else {
                    TypedBinary(region, leftChecked, rightChecked, TypedArithmeticOP.IntModulo)
                }
            }
            case BinaryOP.Equal => {
                boolOrNumberTypes(IntEquals, FloatEquals, BooleanEquals, "equal")
            }
            case BinaryOP.NotEqual => {
                boolOrNumberTypes(IntNotEquals, FloatNotEquals, BooleanNotEquals, "non equals")
            }
            case BinaryOP.GreaterThan => {
                intFloatTypes(IntGreater, TypedArithmeticOP.FloatGreater, "greater than")
            }
            case BinaryOP.LessThan => {
                intFloatTypes(IntLess, TypedArithmeticOP.FloatLess, "less than")
            }
            case BinaryOP.GreaterThanOrEqual => {
                intFloatTypes(IntGreaterEquals, TypedArithmeticOP.FloatGreaterEquals, "greater than or equal")
            }
            case BinaryOP.LessThanOrEqual => {
                intFloatTypes(IntLessEquals, TypedArithmeticOP.FloatLessEquals, "less than or equal")
            }
            case BinaryOP.Concat => {
                resolveExpression(hinting, context, HLCall(region, HLDotName(region, left, "concat"), List(right)))
            }
            case BinaryOP.And => {
                boolTypes(BooleanAnd, "and")
            }
            case BinaryOP.Or => {
                boolTypes(BooleanOr, "or")
            }
        }
    }

    private def checkSuperType(
        context: TypeCheckContext,
        region: Region,
        root: DefaultPackage,
        types: List[Type]
    ): Type = {
        if (types.isEmpty) {
            throw new LanguageException(region, "Cannot check super type of empty list")
        }
        val start = types.head
        val superTypeAll = types.tail.foldLeft(start)((run, tpe) => {
            calculateSuperType(context, region, run, tpe)
        })
        superTypeAll
    }

    private def calculateSuperType(context: TypeCheckContext, region: Region, a: Type, b: Type): Type = {
        calculateSuperTypeOpt(context, region, a, b).getOrElse {
            throw new LanguageTypeException(region, a, b)
        }
    }

    private def calculateSuperTypeOpt(context: TypeCheckContext, region: Region, a: Type, b: Type): Option[Type] = {
        if (Checker.isAssignable(context, a, b, false)) {
            val _ = Checker.isAssignable(context, a, b, true)
            Some(a)
        } else if (Checker.isAssignable(context, b, a, false)) {
            val _ = Checker.isAssignable(context, b, a, true)
            Some(b)
        } else {
            None
        }
    }

    class TypeCheckContext(
        val root: DefaultPackage,
        val selfContext: Option[Type],
        val returnType: Type,
        var variablesTypes: Map[Variable, Type]
    ) {
        def getVariableType(variable: Variable): Type = {
            variablesTypes.get(variable).expectWithRegion(variable.region, "variable type")
        }

        def addVariableType(variable: Variable, tpe: Type): Unit = {
            variablesTypes = variablesTypes + (variable -> tpe)
        }

        def withReturnType(tpe: Type): TypeCheckContext = {
            TypeCheckContext(root, selfContext, tpe, variablesTypes)
        }
    }
}

object ExpressionType {
    def getType(expression: Expression): Type = {
        def getTypeInternal: Type = {
            if (!expression.isInstanceOf[StageTyped]) {
                throw new LanguageException(expression.getRegion, s"Cannot get type ${expression.getClass}")
            }
            expression.asInstanceOf[StageTyped] match {
                case IntLiteral(region, value) => {
                    IntegerType(region)
                }
                case TypedArray(region, tpe, elements) => {
                    ArrayType(region, tpe)
                }
                case FloatLiteral(region, value) => {
                    FloatType(region)
                }
                case TypedVariableLink(region, variable, varType) => {
                    varType
                }
                case TypedNew(region, tpe, elements) => {
                    tpe
                }
                case TypedField(region, obj, _, tpe, name) => {
                    tpe
                }
                case HLArrayIndex(region, left, index) => {
                    val leftType = getType(left)
                    leftType match {
                        case ArrayType(_, tpe) => tpe
                        case _                 => throw new LanguageException(region, "Cannot get type of array index")
                    }
                }
                case BoolLiteral(region, state) => {
                    BooleanType(region)
                }
                case TypedBlock(region, expressions, returnType) => {
                    returnType
                }
                case VarDef(region, variable, typeHint, value) => {
                    VoidType(region)
                }
                case TypedBranch(region, condition, ifTrue, ifFalse) => {
                    if (ifFalse.isDefined) {
                        ifFalse.get._1
                    } else {
                        VoidType(region)
                    }
                }
                case StringLiteral(region, name) => {
                    StringType(region)
                }
                case CallStaticFunction(region, path, returnType, expression) => {
                    returnType
                }
                case TypedClosure(region, parameters, returnType, body) => {
                    FunctionType(region, parameters.map(ref => ref._2), returnType)
                }
                case CallDynamicFunctionIndirect(region, left, returnType, expression) => {
                    returnType
                }
                case CallDynamicFunctionDirect(region, left, path, returnType, expression) => {
                    returnType
                }
                case TypedBinary(region, left, right, op) => {
                    op.getType(region)
                }
                case TypedFor(region, variable, _, iterable, body) => {
                    VoidType(region)
                }
                case Return(region, value) => {
                    VoidType(region)
                }
                case TypedFunction(region, _, obj, _, path, inputTypes, returnType) => {
                    FunctionType(region, inputTypes, returnType)
                }
                case TypedSelf(region, tpe) => {
                    tpe
                }
                case TypedStaticFunction(region, path, inputTypes, returnType) => {
                    FunctionType(region, inputTypes, returnType)
                }
                case While(region, _, _) => VoidType(region)
                case TypedUnary(region, _, op) => {
                    op match {
                        case TypedUnaryArithmeticOP.NEGATE_FLOAT => FloatType(region)
                        case TypedUnaryArithmeticOP.NEGATE_INT   => IntegerType(region)
                        case TypedUnaryArithmeticOP.NOT          => BooleanType(region)
                    }
                }
                case IntToFloat(region, _)                   => FloatType(region)
                case TypedAssignVariable(region, _, _, _)    => VoidType(region)
                case TypedAssignField(region, _, _, _, _, _) => VoidType(region)
                case TypedAssignArray(region, _, _, _)       => VoidType(region)
            }
        }

        @tailrec
        def unravel(tpe: Type): Type = {
            tpe match {
                case resolvable: Resolvable => {
                    if (resolvable.isResolved) {
                        unravel(resolvable.get())
                    } else {
                        resolvable
                    }
                }
                case rest => {
                    rest
                }
            }
        }

        unravel(getTypeInternal)

    }

    def isInt(tpe: Type, context: TypeCheckContext, mutate: Boolean = true): Boolean = {
        Checker.isAssignable(context, IntegerType(tpe.getRegion()), tpe, mutate)
    }

    def isFloat(tpe: Type, context: TypeCheckContext, mutate: Boolean = true): Boolean = {
        Checker.isAssignable(context, FloatType(tpe.getRegion()), tpe, mutate)
    }

    def isBool(tpe: Type, context: TypeCheckContext, mutate: Boolean = true): Boolean = {
        Checker.isAssignable(context, BooleanType(tpe.getRegion()), tpe, mutate)
    }
}

object FlowChecker {

    def getFlowType(expression: Expression): FlowType = {
        if (!expression.isInstanceOf[StageTyped]) {
            throw new LanguageException(expression.getRegion, s"Cannot get type ${expression.getClass}")
        }
        val exprType = FlowType.FlowValue(ExpressionType.getType(expression))
        expression.asInstanceOf[StageTyped] match {
            case Return(region, value) => {
                FlowType.FlowReturn(value.map(ref => ExpressionType.getType(ref)).getOrElse(VoidType(region)))
            }
            case b: TypedBranch => {
                if (b.ifFalse.isDefined) {
                    getFlowType(b.ifTrue) match {
                        case FlowType.FlowReturn(tpe) => {
                            getFlowType(b.ifFalse.get._2) match {
                                case FlowType.FlowReturn(tpe) => {
                                    FlowType.FlowReturn(tpe)
                                }
                                case FlowType.FlowValue(tpe) => {
                                    FlowType.FlowValue(b.ifFalse.get._1)
                                }
                            }
                        }
                        case _ => {
                            FlowType.FlowValue(b.ifFalse.get._1)
                        }
                    }
                } else {
                    FlowType.FlowValue(VoidType(b.getRegion))
                }
            }
            case TypedBlock(region, expressions, returnType) => {
                if (expressions.isEmpty) {
                    FlowType.FlowValue(VoidType(expression.getRegion))
                } else {
                    getFlowType(expressions.last)
                }
            }
            case _ => exprType
        }
    }

    enum FlowType {
        case FlowReturn(tpe: Type)
        case FlowValue(tpe: Type)
    }
}


extension [T](option: Option[T]) {
    def expectWithRegion(region: Region, msg: String): T = option.getOrElse {
        throw new LanguageException(region, s"Expected $msg")
    }
}

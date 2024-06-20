package karina.codegen.rust

import karina.LanguageException
import karina.highlevel.*
import karina.typed.{DefaultClass, DefaultFunction, DefaultPackage, DefaultUnit}
import karina.types.*

object RustCodeGen {

    def generate(root: DefaultPackage, startFn: String): RustFile = {
        val units = root.flatUnits()
        val rustFile = RustFile()
        rustFile.lines = rustFile.lines :+ "#![allow(nonstandard_style)]"
        rustFile.lines = rustFile.lines :+ "#![allow(dead_code)]"
        rustFile.lines = rustFile.lines :+ "#![allow(unused)]"
        generateGC(rustFile, startFn)
        units.foreach(unit => generateUnit(root, unit, rustFile))
        rustFile
    }

    private def generateGC(file: RustFile, startFn: String): Unit = {
        val vector_type = RustType.Literal("Vec<*mut u8>")
        val struct = RustStruct("GC", RustGenerics(List()), List(("objects", vector_type)))
        file.structs = file.structs :+ struct
        file.lines = file.lines :+ "static mut GC_INSTANCE: *mut GC = 0 as *mut GC;"

        val start = RustFunction("main", RustGenerics(List()), List(), RustType.Void, Some(RustLiteral(
            s"""
                |unsafe {
                |   GC_INSTANCE = Box::into_raw(Box::new(GC { objects: Vec::new() }));
                |   $startFn();
                |}
                 """.stripMargin)), true, true)
        file.functions = file.functions :+ start
    }

    private def generateUnit(root: DefaultPackage, unit: DefaultUnit, file: RustFile): Unit = {
        unit.allFunctions.foreach(ref => {
            val function = generateFunction(root, ref, None)
            file.functions = file.functions :+ function
        })
        unit.classes.foreach(ref => {
            val (struct, functions) = generateClass(root, ref)
            file.structs = file.structs :+ struct
            file.functions = file.functions ++ functions
        })
    }

    private def generateFunction(root: DefaultPackage, function: DefaultFunction, self: Option[Type]): RustFunction = {
        val args = function.parameters.map(ref => {
            (ref.name, toRustType(ref.tpe))
        }) ++ self.map(tpe => ("this", toRustType(tpe)))
        val ret = toRustType(function.returnType)
        val generics = RustGenerics(function.definedGenerics.map(ref => ref.name))
        RustFunction(function.name(), generics, args, ret, Some(toRustExpression(root, function.body)))
    }

    private def generateClass(root: DefaultPackage, struct: DefaultClass): (RustStruct, List[RustFunction]) = {
        val fields = struct.fields.map(ref => (ref.name, toRustType(ref.tpe)))
        val generics = RustGenerics(struct.genericDefinition.map(ref => ref.name))
        val name = struct.path.mkString("_")
        val selfType = ObjectType(struct.region, struct.path, Map.empty)
        val functions = struct.functions.map(ref => generateFunction(root, ref, Some(selfType)))
        (RustStruct(name, generics, fields), functions)
    }

    private def toRustType(tpe: Type): RustType = {
        tpe match {
            case ObjectTypeDefault(region, path, generics) => ???
            case GenericType(region, name, source)         => ???
            case FunctionType(region, args, ret)           => ???
            case ArrayType(region, t)                      => ???
            case FloatType(region)                         => RustType.Float
            case VoidType(region)                          => RustType.Void
            case IntegerType(region)                       => RustType.Int
            case StringType(region)                        => ???
            case BooleanType(region)                       => RustType.Bool
            case BaseType(region)                          => ???
            case resolvable: Resolvable                    => ???
            case ObjectType(region, path, binds)           => {
                val name = path.mkString("_");
                val types = binds.map(ref => toRustType(ref._2)).toList
                RustType.Object(name, types)
            }
            case EnumType(region, path, binds)             => ???
            case EnumCaseType(region, owner, path)         => ???
        }
    }

    private def toRustExpression(root: DefaultPackage, expression: Expression): RustExpression = {
        if (!expression.isInstanceOf[StageTyped]) {
            throw new LanguageException(expression.getRegion, s"Cannot parse stage ${expression.getClass}")
        }
        expression.asInstanceOf[StageTyped] match {
            case While(region, condition, body) => {

                
            }
            case FloatLiteral(region, value) => {
                RustFloat(value)
            }
            case IntLiteral(region, value) => {
                RustInt(value)
            }
            case BoolLiteral(region, state) => {
                RustBool(state)
            }
            case StringLiteral(region, name) => ???
            case Return(region, value) => {
                value match {
                    case Some(expr) => RustReturnValue(toRustExpression(root, expr))
                    case None => RustReturn()
                }
            }
            case HLArrayIndex(region, left, index) => ???
            case VarDef(region, variable, typeHint, value) => {
                RustLet(variable.name, toRustType(typeHint.get), toRustExpression(root, value))
            }
            case TypedArray(region, tpe, elements) => ???
            case TypedNewObject(region, tpe, elements) => {
                val fields = elements.map(ref => (ref._1, toRustExpression(root, ref._2)))
                RustObject(tpe.path.mkString("_"), fields)
            }
            case TypedNewEnum(region, tpe, elements) => ???
            case TypedField(region, objType, obj, fieldType, field) => {
                RustGetField(toRustExpression(root, obj), field)
            }
            case TypedBranch(region, condition, ifTrue, ifFalse) => ???
            case TypedBlock(region, expressions, returnType) => {
                RustBlock(expressions.map(ref => toRustExpression(root, ref)))
            }
            case CallStaticFunction(region, path, returnType, expression) => ???
            case CallDynamicFunctionIndirect(region, left, returnType, expression) => ???
            case CallDynamicFunctionDirect(region, obj, path, returnType, expression) => ???
            case TypedStaticFunction(region, path, inputTypes, returnType) => ???
            case TypedFunction(region, objType, obj, name, path, inputTypes, returnType) => ???
            case TypedBinary(region, left, right, op) => {
                val symbol = op.toSymbol()
                RustBinary(toRustExpression(root, left), toRustExpression(root, right), symbol)
            }
            case TypedUnary(region, left, op) => ???
            case IntToFloat(region, value) => ???
            case TypedFor(region, variable, varType, iterable, body) => ???
            case TypedSelf(region, tpe) => ???
            case TypedAssignVariable(region, variable, varType, value) => ???
            case TypedVariableLink(region, variable, varType) => {
                RustLiteral(variable.name)
            }
            case TypedAssignField(region, objType, obj, fieldType, field, value) => ???
            case TypedAssignArray(region, obj, index, value) => ???
            case TypedClosure(region, parameters, returnType, body) => ???
        }
    }
}

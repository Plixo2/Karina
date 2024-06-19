package karina.codegen.bin

import karina.LanguageException
import karina.codegen.ObjectRepresentation
import karina.codegen.ObjectRepresentation.MemoryObject
import karina.codegen.bin.BinCodeGen.Instruction
import karina.codegen.bin.BinCodeGen.Instruction.{Jump, JumpWithValue, PutArray, UnInstruction}
import karina.highlevel.*
import karina.typed.FlowChecker.FlowType
import karina.typed.{DefaultFunction, DefaultPackage, DefaultUnit, FlowChecker}
import karina.types.VoidType

object BinCodeGen {
    private type Register = Int

    private class RegisterPage(var limit: Int) {
        private var usedRegisters: Set[Register] = Set()
        private var permanentRegisters: Set[Register] = Set()
        private var maxUsed: Int = 0

        def allocate(): Register = {
            val reg = (0 until limit)
                .find(!usedRegisters.contains(_))
                .map { i =>
                    usedRegisters += i
                    i
                }
                .getOrElse(throw new Exception("Out of registers"))
            maxUsed = reg.max(maxUsed)
            reg
        }

        def free(register: Register): Unit = {
            if (!permanentRegisters.contains(register)) {
                usedRegisters -= register
            }
        }

        def addPermanent(register: Register): Unit = {
            permanentRegisters += register
            maxUsed = register.max(maxUsed)
        }
        def removePermanent(register: Register): Unit = {
            permanentRegisters -= register
        }

        def maxUsedRegister(): Int = maxUsed + 1
    }

    case class CodeContainer(functions: List[CodeFunction], objects: List[MemoryObject]) {
        def addFunction(function: CodeFunction): CodeContainer = {
            CodeContainer(functions :+ function, objects)
        }
        def addObject(obj: MemoryObject): CodeContainer = {
            CodeContainer(functions, objects :+ obj)
        }
    }

    case class CodeFunction(
        name: String,
        registers: Int,
        returnType: String,
        parameters: List[String],
        instructions: List[Instruction],
        variables: Map[Variable, Register]
    ) {
        def addInstruction(instruction: Instruction): CodeFunction = {
            CodeFunction(name, registers, returnType, parameters, instructions :+ instruction, variables)
        }
        def addLabel(name: String, args: List[Register] = List()): CodeFunction = {
            addInstruction(Instruction.Label(name, args))
        }
        def addVariable(variable: Variable, register: Register): CodeFunction = {
            CodeFunction(name, registers, returnType, parameters, instructions, variables + (variable -> register))
        }

        def setMaxUsedRegisters(max: Int): CodeFunction = {
            CodeFunction(name, max, returnType, parameters, instructions, variables)
        }

        def removeInstructions(): CodeFunction = {
            CodeFunction(name, registers, returnType, parameters, List(), variables)
        }
    }

    enum Instruction {
        case Debug(reg: Register)
        case Label(name: String, args: List[Register])
        case LoadInt(reg: Register, value: Long)
        case Assign(dest: Register, src: Register)
        case DefVar(dest: Register, src: Register, tpe: String)
        case LoadFloat(reg: Register, value: Double)
        case LoadBool(reg: Register, state: Boolean)
        case LoadString(reg: Register, value: String)
        case BinInstruction(reg: Register, left: Register, right: Register, instruction: TypedArithmeticOP)
        case UnInstruction(reg: Register, left: Register, instruction: TypedUnaryArithmeticOP)
        case ReturnValue(reg: Register)
        case Return()
        case GetField(reg: Register, obj: Register, offset: Int)
        case PutField(obj: Register, offset: Int, value: Register)
        case GetDynamicFunctionPointer(reg: Register, obj: Register, offset: Int)
        case GetStaticFunctionPointer(reg: Register, path: String)
        case ArrayIndex(reg: Register, obj: Register, index: Register)
        case Jump(target: String)
        case JumpWithValue(target: String, obj: Register)
        case JumpIfTrue(ifTrue: String, ifFalse: String, obj: Register)
        case JumpIfTrueWithValue(ifTrue: String, ifFalse: String, obj: Register)
        case JumpIfFalse(target: String, obj: Register)
        case IntToFloat(reg: Register, obj: Register)
        case NewArray(reg: Register, args: List[Register], tpe: String)
        case PutArray(obj: Register, index: Register, value: Register)
        case NewObject(reg: Register, tpe: String, args: List[Register])
        case CallStatic(reg: Register, function: String, args: List[Register])
        case CallDynamic(reg: Register, function: Register, args: List[Register])
        case CallDynamicDirect(reg: Register, function: String, args: List[Register])
        case CallNative(reg: Register, name: String, args: List[Register])

    }

    def generate(root: DefaultPackage): CodeContainer = {
        val units = root.flatUnits()
        units.foldLeft(CodeContainer(List(), List())) { (container, unit) =>
            generateUnit(root, unit, container)
        }
    }
    private def generateUnit(root: DefaultPackage, unit: DefaultUnit, container: CodeContainer): CodeContainer = {

        val units = unit.functions().foldLeft(container) { (container, function) =>
            container.addFunction(generateFunction(root, function, false))
        }
        unit.classes.foldLeft(units) { (container, clazz) =>
            val addedFunctions = clazz.functions.foldLeft(container) { (container, function) =>
                container.addFunction(generateFunction(root, function, true))
            }
            addedFunctions.addObject(ObjectRepresentation.loadFromObj(clazz))

        }

    }

    private def generateFunction(root: DefaultPackage, function: DefaultFunction, useSelf: Boolean): CodeFunction = {
        val params = function.parameters.map(ref => ref.tpe.getLowLevelType())
        val types = if (useSelf) {
            "p" +: params
        } else {
            params
        }

        var codeFunction = CodeFunction(
          function.path.mkString("."),
          0,
          function.returnType.getLowLevelType(),
          types,
          List(),
          Map()
        )
        val registerPage = RegisterPage(limit = 1023)
        if (useSelf) {
            val register = registerPage.allocate()
            registerPage.addPermanent(register)
        }
        function.parameters.foreach { parameter =>
            val register = registerPage.allocate()
            registerPage.addPermanent(register)
            codeFunction = codeFunction.addVariable(parameter.variableObject.get, register)
        }
        val codeFn = if (function.modifier == Modifier.Native) {
            val params = function.parameters.indices.toList
            val register = registerPage.allocate()
            var eval =
                codeFunction.addInstruction(Instruction.CallNative(register, function.path.mkString("."), params))
            eval = eval.addInstruction(Instruction.ReturnValue(register))
            eval
        } else {
            generateExpression(root, function.body, codeFunction, registerPage)._1
        }
        codeFn.setMaxUsedRegisters(registerPage.maxUsedRegister())
    }

    private def generateExpression(
        root: DefaultPackage,
        expression: Expression,
        container: CodeFunction,
        registerPage: RegisterPage
    ): (CodeFunction, Register) = {
        if (!expression.isInstanceOf[StageTyped]) {
            throw new LanguageException(expression.getRegion, s"Cannot parse stage ${expression.getClass}")
        }
        expression.asInstanceOf[StageTyped] match {
            case While(region, condition, body) => {
                val label = s"while_${container.instructions.length}"
                val labelEnd = s"while_end_${container.instructions.length}"
                val whileStart = container.addInstruction(Instruction.Label(label, List()))
                val (fn, regCond) = generateExpression(root, condition, whileStart, registerPage)
                registerPage.free(regCond)
                val jumps = fn.addInstruction(Instruction.JumpIfFalse(labelEnd, regCond))
                val (fn2, regBody) = generateExpression(root, body, jumps, registerPage)
                registerPage.free(regBody)
                val instruction2 = Instruction.Jump(label)
                (fn2.addInstruction(instruction2).addInstruction(Instruction.Label(labelEnd, List())), -1)
            }
            case FloatLiteral(region, value) => {
                val reg = registerPage.allocate()
                (container.addInstruction(Instruction.LoadFloat(reg, value)), reg)
            }
            case IntLiteral(region, value) => {
                val reg = registerPage.allocate()
                (container.addInstruction(Instruction.LoadInt(reg, value)), reg)
            }
            case BoolLiteral(region, state) => {
                val reg = registerPage.allocate()
                (container.addInstruction(Instruction.LoadBool(reg, state)), reg)
            }
            case StringLiteral(region, name) => {
                val reg = registerPage.allocate()
//                val escaped = StringEscapeUtils.escapeJava(name)
                (container.addInstruction(Instruction.LoadString(reg, name)), reg)
            }
            case Return(region, value) => {
                value match {
                    case Some(value) => {
                        val (fn, reg) = generateExpression(root, value, container, registerPage)
                        registerPage.free(reg)
                        (fn.addInstruction(Instruction.ReturnValue(reg)), -1)
                    }
                    case None => {
                        (container.addInstruction(Instruction.Return()), -1)
                    }
                }
            }
            case HLArrayIndex(region, left, index) => {
                val (fn, regLeft) = generateExpression(root, left, container, registerPage)
                val (fn2, regIndex) = generateExpression(root, index, fn, registerPage)
                registerPage.free(regLeft)
                registerPage.free(regIndex)
                val reg = registerPage.allocate()
                (fn2.addInstruction(Instruction.ArrayIndex(reg, regLeft, regIndex)), reg)
            }
            case TypedVariableLink(region, variable, _) => {
                val reg = container.variables(variable)
                (container, reg)
            }
            case VarDef(region, variable, typeHint, value) => {
                val (fn, reg) = generateExpression(root, value, container, registerPage)
                val tpe = typeHint.get.getLowLevelType()
                registerPage.free(reg)
                val register = registerPage.allocate()
                registerPage.addPermanent(register)
                (fn.addVariable(variable, register).addInstruction(Instruction.DefVar(register, reg, tpe)), register)
            }
            case TypedArray(region, tpe, elements) => {
                var registers = List[Register]()
                val codeFunction = elements.foldLeft(container) { (container, expr) =>
                    val (fn, reg) = generateExpression(root, expr, container, registerPage)
                    registers = registers :+ reg
                    fn
                }
                registers.foreach(registerPage.free)
                val reg = registerPage.allocate()
                val instruction = Instruction.NewArray(reg, registers, tpe.getLowLevelType())
                (codeFunction.addInstruction(instruction), reg)
            }
            case TypedNew(region, tpe, elements) => {
                var registers = List[Register]()
                val codeFunction = elements.foldLeft(container) { (container, expr) =>
                    val (fn, reg) = generateExpression(root, expr, container, registerPage)
                    registers = registers :+ reg
                    fn
                }
                val reg = registerPage.allocate()
                registers.foreach(registerPage.free)
                val instruction = Instruction.NewObject(reg, tpe.path.mkString("."), registers)
                (codeFunction.addInstruction(instruction), reg)
            }
            case TypedField(region, objType, obj, fieldType, field) => {
                val memObj = ObjectRepresentation.load(root, objType)
                val offset = memObj.getSlot(field)
                if (offset == -1) {
                    throw new LanguageException(region, s"Field $field not found")
                }

                val (fn, regObj) = generateExpression(root, obj, container, registerPage)
                registerPage.free(regObj)
                val reg = registerPage.allocate()
                val instruction = Instruction.GetField(reg, regObj, offset)
                (fn.addInstruction(instruction), reg)
            }
            case TypedBranch(region, condition, ifTrue, ifFalse) => {
                // TODO fix this fucking mess
                val trueCaseLabel = s"true_case${container.instructions.length}"
                val falseCaseLabel = s"false_case${container.instructions.length}"
                val endLabel = s"end_case${container.instructions.length}"
                val (fn, regCond) = generateExpression(root, condition, container, registerPage)
                val doesYieldValue = ifFalse.exists(ref => !ref._1.isInstanceOf[VoidType])
                if (!doesYieldValue) {
                    val flowType = FlowChecker.getFlowType(ifTrue)
                    val jumpFn =
                        fn.addInstruction(Instruction.JumpIfTrue(trueCaseLabel, falseCaseLabel, regCond))
                    registerPage.free(regCond)
                    val trueLabelFn = jumpFn.addLabel(trueCaseLabel)
                    val (compiledThen, reg) = generateExpression(root, ifTrue, trueLabelFn, registerPage)
                    registerPage.free(reg)
                    flowType match {
                        case FlowType.FlowReturn(tpe) => {
                            val falseStart = compiledThen.addLabel(falseCaseLabel)
                            if (ifFalse.isDefined) {
                                val (compiledElse, reg) =
                                    generateExpression(root, ifFalse.get._2, falseStart, registerPage)
                                registerPage.free(reg)
                                val flowType = FlowChecker.getFlowType(ifFalse.get._2)
                                flowType match {
                                    case FlowType.FlowReturn(tpe) => {
                                        (compiledElse, -1)
                                    }
                                    case FlowType.FlowValue(tpe) => {
                                        (compiledElse.addInstruction(Instruction.Jump(endLabel)).addLabel(endLabel), -1)
                                    }
                                }
                            } else {
                                (falseStart, -1)
                            }
                        }
                        case FlowType.FlowValue(tpe) => {
                            val falseStart = compiledThen.addInstruction(Jump(endLabel)).addLabel(falseCaseLabel)
                            if (ifFalse.isDefined) {
                                val (compiledElse, reg) =
                                    generateExpression(root, ifFalse.get._2, falseStart, registerPage)
                                registerPage.free(reg)
                                val flowType = FlowChecker.getFlowType(ifFalse.get._2)
                                flowType match {
                                    case FlowType.FlowReturn(tpe) => {
                                        (compiledElse.addLabel(endLabel), -1)
                                    }
                                    case FlowType.FlowValue(tpe) => {
                                        (compiledElse.addInstruction(Instruction.Jump(endLabel)).addLabel(endLabel), -1)
                                    }
                                }
                            } else {
                                (falseStart.addLabel(endLabel), -1)
                            }
                        }
                    }
                } else {
                    assert(ifFalse.isDefined)
                    val isFalse = ifFalse.get._2
                    val flowTypeTrue = FlowChecker.getFlowType(ifTrue)
                    val flowTypeFalse = FlowChecker.getFlowType(isFalse)
                    flowTypeTrue match {
                        case FlowType.FlowReturn(tpe) => {
                            flowTypeFalse match {
                                case FlowType.FlowReturn(tpe) => {
                                    throw new LanguageException(
                                      region,
                                      "Cannot have return in both branches for yield values"
                                    )
                                }
                                case FlowType.FlowValue(tpe) => {
                                    val fnJump = fn
                                        .addInstruction(
                                          Instruction.JumpIfTrueWithValue(trueCaseLabel, falseCaseLabel, regCond)
                                        )
                                        .addLabel(trueCaseLabel)
                                    val (compiledThen, reg) = generateExpression(root, ifTrue, fnJump, registerPage)
                                    registerPage.free(reg)
                                    val falseStart = compiledThen.addLabel(falseCaseLabel)
                                    val (compiledElse, regA) =
                                        generateExpression(root, isFalse, falseStart, registerPage)
                                    val target = registerPage.allocate()
                                    (
                                      compiledElse.addInstruction(JumpWithValue(endLabel, regA)).addLabel(endLabel, List(target)),
                                      target
                                    )
                                }
                            }
                        }
                        case FlowType.FlowValue(tpe) => {
                            flowTypeFalse match {
                                case FlowType.FlowReturn(tpe) => {
                                    val fnJump = fn
                                        .addInstruction(
                                          Instruction.JumpIfTrueWithValue(trueCaseLabel, falseCaseLabel, regCond)
                                        )
                                        .addLabel(trueCaseLabel)
                                    val (compiledThen, reg) = generateExpression(root, ifTrue, fnJump, registerPage)
                                    val falseStart = compiledThen.addInstruction(JumpWithValue(endLabel, reg)).addLabel(falseCaseLabel)
                                    val (compiledElse, regA) = generateExpression(root, isFalse, falseStart, registerPage)
                                    registerPage.free(regA)
                                    val target = registerPage.allocate()
                                    (
                                      compiledElse.addLabel(endLabel, List(target)),
                                      target
                                    )
                                }
                                case FlowType.FlowValue(tpe) => {
                                    val fnJump = fn
                                        .addInstruction(
                                            Instruction.JumpIfTrueWithValue(trueCaseLabel, falseCaseLabel, regCond)
                                        )
                                        .addLabel(trueCaseLabel)
                                    val (compiledThen, reg) = generateExpression(root, ifTrue, fnJump, registerPage)
                                    val falseStart = compiledThen.addInstruction(JumpWithValue(endLabel, reg)).addLabel(falseCaseLabel)
                                    val (compiledElse, regA) = generateExpression(root, isFalse, falseStart, registerPage)
                                    val target = registerPage.allocate()
                                    (
                                        compiledElse.addInstruction(JumpWithValue(endLabel, regA)).addLabel(endLabel, List(target)),
                                        target
                                    )
                                }
                            }
                        }
                    }

                }
            }
            case TypedBlock(region, expressions, returnType) => {
                expressions.foldLeft((container, -1)) { (container, expression) =>
                    registerPage.free(container._2)
                    val (fn, reg) = generateExpression(root, expression, container._1, registerPage)
                    (fn, reg)
                }
            }
            case CallStaticFunction(region, path, returnType, expression) => {
                var registers = List[Register]()
                val codeFunction = expression.foldLeft(container) { (container, expr) =>
                    val (fn, reg) = generateExpression(root, expr, container, registerPage)
                    registers = registers :+ reg
                    fn
                }
                val reg = registerPage.allocate()
                val instruction = Instruction.CallStatic(reg, path.mkString("."), registers)
                registers.foreach(registerPage.free)
                (codeFunction.addInstruction(instruction), reg)
            }
            case CallDynamicFunctionDirect(region, obj, path, returnType, expression) => {
                val (fn, regLeft) = generateExpression(root, obj, container, registerPage)
                var registers = List[Register](regLeft)
                val codeFunction = expression.foldLeft(fn) { (container, expr) =>
                    val (fn, reg) = generateExpression(root, expr, container, registerPage)
                    registers = registers :+ reg
                    fn
                }
                registers.foreach(registerPage.free)
                val reg = registerPage.allocate()
                val instruction = Instruction.CallDynamicDirect(reg, path.mkString("."), registers)
                (codeFunction.addInstruction(instruction), reg)
            }
            case CallDynamicFunctionIndirect(region, left, returnType, expression) => {
                val (fn, regLeft) = generateExpression(root, left, container, registerPage)
                var registers = List[Register]()
                val codeFunction = expression.foldLeft(fn) { (container, expr) =>
                    val (fn, reg) = generateExpression(root, expr, container, registerPage)
                    registers = registers :+ reg
                    fn
                }
                registers.foreach(registerPage.free)
                val reg = registerPage.allocate()
                val instruction = Instruction.CallDynamic(reg, regLeft, registers)
                registerPage.free(regLeft)
                (codeFunction.addInstruction(instruction), reg)
            }
            case TypedStaticFunction(region, path, inputTypes, returnType) => {
                val reg = registerPage.allocate()
                val instruction = Instruction.GetStaticFunctionPointer(reg, path.mkString("."))
                (container.addInstruction(instruction), reg)
            }
            case TypedFunction(region, objType, obj, name, path, inputTypes, returnType) => {
                val memObj = ObjectRepresentation.load(root, objType)
                val offset = memObj.getSlot(name)
                if (offset == -1) {
                    throw new LanguageException(region, s"Function $name not found")
                }

                val (fn, regObj) = generateExpression(root, obj, container, registerPage)
                registerPage.free(regObj)
                val reg = registerPage.allocate()
                val instruction = Instruction.GetDynamicFunctionPointer(reg, regObj, offset)
                (fn.addInstruction(instruction), reg)
            }
            case TypedBinary(region, left, right, op) => {
                val (fn, regLeft) = generateExpression(root, left, container, registerPage)
                val (fn2, regRight) = generateExpression(root, right, fn, registerPage)
                registerPage.free(regLeft)
                registerPage.free(regRight)
                val reg = registerPage.allocate()
                (fn2.addInstruction(Instruction.BinInstruction(reg, regLeft, regRight, op)), reg)
            }
            case TypedUnary(region, left, op) => {
                val (fn, regLeft) = generateExpression(root, left, container, registerPage)
                registerPage.free(regLeft)
                val reg = registerPage.allocate()
                (fn.addInstruction(Instruction.UnInstruction(reg, regLeft, op)), reg)
            }
            case IntToFloat(region, value) => {
                val (fn, right) = generateExpression(root, value, container, registerPage)
                registerPage.free(right)
                val reg = registerPage.allocate()
                (fn.addInstruction(Instruction.IntToFloat(reg, right)), reg)
            }
            case TypedFor(region, variable, _, iterable, body) => {
                throw new LanguageException(region, "For loops are not supported in binary code generation")
            }
            case TypedSelf(region, tpe) => {
                (container, 0)
            }
            case TypedClosure(region, parameters, returnType, body) => {
                throw new LanguageException(region, "Closures are not supported in binary code generation")
            }
            case TypedAssignVariable(region, variable, _, value) => {
                val reg = container.variables(variable)
                val (fn, right) = generateExpression(root, value, container, registerPage)
                registerPage.free(right)
                (fn.addInstruction(Instruction.Assign(reg, right)), -1)
            }
            case TypedAssignField(region, objType, obj, fieldType, field, value) => {
                val memObj = ObjectRepresentation.load(root, objType)
                val offset = memObj.getSlot(field)
                if (offset == -1) {
                    throw new LanguageException(region, s"Field $field not found")
                }

                val (fn, regObj) = generateExpression(root, obj, container, registerPage)
                val (fn2, valueReg) = generateExpression(root, value, fn, registerPage)
                registerPage.free(regObj)
                registerPage.free(valueReg)
                val instruction = Instruction.PutField(regObj, offset, valueReg)
                (fn2.addInstruction(instruction), -1)
            }
            case TypedAssignArray(region, obj, index, value) => {
                val (fn, right) = generateExpression(root, value, container, registerPage)
                val (fn2, left) = generateExpression(root, obj, fn, registerPage)
                val (fn3, indexReg) = generateExpression(root, index, fn2, registerPage)
                registerPage.free(right)
                registerPage.free(left)
                registerPage.free(indexReg)
                (fn3.addInstruction(PutArray(left, indexReg, right)), -1)
            }
        }

    }

}

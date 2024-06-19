package karina.codegen.bin

import karina.codegen.bin.BinCodeGen.Instruction.{JumpIfFalse, Label}
import karina.codegen.bin.BinCodeGen.{CodeContainer, CodeFunction, Instruction}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

object BinPrinterPlain {

    def write(main: String, container: CodeContainer, path: String): Unit = {
        val objs = container.objects
            .map(ref => {
                s"obj ${ref.name} [${ref.gcMask}] { " + ref.slots.mkString(", ") + " }"
            })
            .mkString("\n") + "\n"

        val content = container.functions
            .map { function =>
                functionToString(function).mkString("", "\n    ", "\n}")
            }
            .mkString("\n\n\n")

        val start =
            s"""
              |fn fn r1 ():i {
              |    call_static 0 ${main} []
              |    exit 0
              |}
              |""".stripMargin

        val filePath = Paths.get(path)
        Files.write(filePath, (start + objs + content).getBytes(StandardCharsets.UTF_8))

    }

    private def functionToString(function: CodeFunction): List[String] = {
        val seq = List[String]() :+ s"fn ${function.name} r${function.registers} (${
                function.parameters.mkString(", ")}):${function.returnType} {"

        def printInstructions(seq: List[String], instructions: List[Instruction]): List[String] = {
            instructions.foldLeft(seq) { (seq, instruction) =>
                instruction match {
                    case Instruction.Debug(reg) => {
                        seq :+ s"debug $reg"
                    }
                    case Instruction.LoadInt(reg, value) => {
                        seq :+ s"load_int $reg $value"
                    }
                    case Instruction.LoadFloat(reg, value) => {
                        seq :+ s"load_float $reg $value"
                    }
                    case Instruction.LoadBool(reg, value) => {
                        seq :+ s"load_bool $reg $value"
                    }
                    case Instruction.BinInstruction(reg, left, right, instruction) => {
                        seq :+ s"binary ${instruction.toString} $reg $left $right"
                    }
                    case Instruction.UnInstruction(reg, left, instruction) => {
                        seq :+ s"unary ${instruction.toString} $reg $left"
                    }
                    case Instruction.LoadString(reg, value) => {
                        seq :+ s"load_string $reg $value"
                    }
                    case Instruction.ReturnValue(reg) => {
                        seq :+ s"return_value $reg"
                    }
                    case Instruction.Return() => {
                        seq :+ "return"
                    }
                    case Instruction.GetField(reg, obj, offset) => {
                        seq :+ s"get_field $reg $obj $offset"
                    }

                    case Instruction.GetDynamicFunctionPointer(reg, obj, offset) => {
                        seq :+ s"get_dynamic_function_pointer $reg $obj $offset"
                    }
                    case Instruction.GetStaticFunctionPointer(reg, path) => {
                        seq :+ s"get_static_function_pointer $reg $path"
                    }
                    case Instruction.ArrayIndex(reg, obj, index) => {
                        seq :+ s"array_index $reg $obj $index"
                    }
                    case Instruction.Jump(target) => {
                        seq :+ s"jump $target"
                    }
                    case Instruction.JumpIfFalse(target, obj) => {
                        seq :+ s"jump_if_not $target $obj"
                    }
                    case Instruction.JumpIfTrue(thenLabel, falseTarget, obj) => {
                        seq :+ s"jump_if $obj $thenLabel $falseTarget"
                    }
                    case Instruction.JumpIfTrueWithValue(thenLabel, falseTarget, obj) => {
                        seq :+ s"jump_if_v $obj $thenLabel $falseTarget"
                    }
                    case Instruction.JumpWithValue(target, obj) => {
                        seq :+ s"jump_v $target $obj"
                    }
                    case Instruction.IntToFloat(reg, obj) => {
                        seq :+ s"int_to_float $reg $obj"
                    }
                    case Instruction.NewArray(reg, args, tpe) => {
                        seq :+ s"new_array $reg [${args.mkString(", ")}] $tpe"
                    }
                    case Instruction.PutArray(reg, index, obj) => {
                        seq :+ s"put_array $reg $index $obj"
                    }
                    case Instruction.CallStatic(reg, function, args) => {
                        seq :+ s"call_static $reg $function [${args.mkString(", ")}]"
                    }
                    case Instruction.CallDynamic(reg, function, args) => {
                        seq :+ s"call_dynamic $reg $function [${args.mkString(", ")}]"
                    }
                    case Instruction.CallDynamicDirect(reg, path, args) => {
                        seq :+ s"call_dynamic_direct $reg $path [${args.mkString(", ")}]"
                    }
                    case Instruction.CallNative(reg, name, args) => {
                        seq :+ s"call_native $reg $name [${args.mkString(", ")}]"
                    }
                    case Instruction.NewObject(reg, path, args) => {
                        seq :+ s"new_object $reg $path [${args.mkString(", ")}]"
                    }
                    case Instruction.Assign(reg, obj) => {
                        seq :+ s"assign $reg $obj"
                    }
                    case Instruction.PutField(obj, offset, value) => {
                        seq :+ s"put_field $obj $offset $value"
                    }
                    case Instruction.DefVar(reg, value, tpe) => {
                        seq :+ s"def_var $reg $value $tpe"
                    }
                    case Label(name, lst) => {
                        seq :+ s"#$name [${lst.mkString(", ")}]"
                    }

                }
            }
        }
        printInstructions(seq, function.instructions)
    }

}

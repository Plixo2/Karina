package karina.codegen.rust


import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

object RustPrinter {


    def write(rustFile: RustFile, path: String): Unit = {
        val content = rustFile.lines.mkString("\n") + "\n\n" + rustFile.functions.map(ref => {
            functionToString(ref)
        }).mkString("\n\n") + "\n\n" + rustFile.structs.map(ref => {
            structToString(ref)
        }).mkString("\n\n")
        val filePath = Paths.get(path)
        Files.write(filePath, content.getBytes(StandardCharsets.UTF_8))
    }

    private def functionToString(rustFunction: RustFunction): String = {
        val genericString = if (rustFunction.generics.names.isEmpty) {
            ""
        } else {
            "<" + rustFunction.generics.names.mkString(", ") + ">"
        }
        val args = rustFunction.args.map(ref => {
            s"${ref._1}: ${rustTypeToString(ref._2)}"
        }).mkString(", ")
        val ret = rustTypeToString(rustFunction.returnType)
        val body = rustExpressionToString(RustBlock(rustFunction.body.toList))
        var prefix = ""
        if (rustFunction.isPublic) {
            prefix = "pub "
        }
        if (!rustFunction.isSafe) {
            prefix = prefix + "unsafe "
        }
        s"${prefix}fn ${rustFunction.name}$genericString($args) -> $ret $body"
    }

    private def structToString(rustStruct: RustStruct): String = {
        val genericString = if (rustStruct.generics.names.isEmpty) {
            ""
        } else {
            "<" + rustStruct.generics.names.mkString(", ") + ">"
        }
        val fields = rustStruct.members.map(ref => {
            s"${ref._1}: ${rustTypeToString(ref._2)}"
        }).mkString(",\n")
        s"struct ${rustStruct.name}$genericString {\n$fields\n}"
    }


    private def rustTypeToString(rustType: RustType): String = {
        rustType match {
            case RustType.Int => "i64"
            case RustType.Float => "f64"
            case RustType.Bool => "bool"
            case RustType.Void => "()"
            case RustType.Literal(value) => value
            case RustType.Object(name, generics) => {
                "*mut " + name + {
                    if (generics.isEmpty) {
                        ""
                    } else {
                        "<" + generics.map(ref => rustTypeToString(ref)).mkString(", ") + ">"
                    }
                }
            }
            case _ => ???
        }
    }

    private def rustExpressionToString(expr: RustExpression): String = {
        expr match {
            case RustLiteral(value) => value
            case RustFloat(value) => s"${value.toString}f64"
            case RustInt(value) => s"${value.toString}i64"
            case RustBool(value) => value.toString
            case RustLet(name, tpe, value) => s"let $name: ${rustTypeToString(tpe)} = ${rustExpressionToString(value)}"
            case RustBinary(left, right, op) => s"(${rustExpressionToString(left)}) $op (${rustExpressionToString(right)})"
            case RustReturnValue(value) => s"return ${rustExpressionToString(value)}"
            case RustReturn() => "return"
            case RustGetField(obj, name) => s"(*${rustExpressionToString(obj)}).$name"
            case RustBlock(statements) => s"{\n${statements.map(ref => rustExpressionToString(ref)).mkString(";\n")};\n}"
            case RustObject(name, members) => {
                val memberStr = members.map(ref => {
                    s"${ref._1}: ${rustExpressionToString(ref._2)}"
                }).mkString(", ")
                s"{ let ptr = Box::into_raw(Box::new($name { $memberStr })); \n " +
                    s"(*GC_INSTANCE).objects.push(ptr as *mut u8); \n ptr } "
            }
        }
    }
}

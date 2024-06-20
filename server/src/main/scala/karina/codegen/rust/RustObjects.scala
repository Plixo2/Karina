package karina.codegen.rust

class RustFile {
    var functions: List[RustFunction] = List()
    var structs: List[RustStruct] = List()
    var lines: List[String] = List()
}

enum RustType {
    case Int
    case Void
    case Float
    case Bool
    case Literal(value: String)
    case Object(name: String, binds: List[RustType])
}

case class RustFunction(name: String, generics: RustGenerics, args: List[(String, RustType)], returnType: RustType, 
                        body: Option[RustExpression], isPublic: Boolean = false, isSafe: Boolean = false)

case class RustStruct(name: String, generics: RustGenerics, members: List[(String, RustType)])

case class RustGenerics(names: List[String])



sealed trait RustExpression {
    
}

case class RustFloat(value: Double) extends RustExpression
case class RustInt(value: Long) extends RustExpression
case class RustBool(value: Boolean) extends RustExpression
case class RustLet(name: String, tpe: RustType, value: RustExpression) extends RustExpression
case class RustBinary(left: RustExpression, right: RustExpression, op: String) extends RustExpression
case class RustReturnValue(value: RustExpression) extends RustExpression
case class RustReturn() extends RustExpression
case class RustBlock(statements: List[RustExpression]) extends RustExpression
case class RustLiteral(value: String) extends RustExpression
case class RustGetField(obj: RustExpression, name: String) extends RustExpression

case class RustObject(name: String, members: List[(String, RustExpression)]) extends RustExpression

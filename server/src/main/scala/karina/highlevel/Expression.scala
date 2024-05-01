package karina.highlevel

import karina.lexer.Region
import karina.typed.Variable
import karina.types.Type

sealed trait Stage {}

sealed trait StageNode extends Stage {}

sealed trait StageVariable extends Stage {}
sealed abstract class Expression(region: Region) {
    def getRegion: Region = region
}

case class HLWhile(region: Region, condition: Expression, body: Expression) extends Expression(region) with StageNode
case class HLFor(region: Region, variable: String, iterable: Expression, body: Expression)
    extends Expression(region)
    with StageNode
case class HLBlock(region: Region, expressions: List[Expression]) extends Expression(region) with StageNode

case class FloatLiteral(region: Region, value: Double) extends Expression(region) with StageNode with StageVariable
case class IntLiteral(region: Region, value: Long) extends Expression(region) with StageNode with StageVariable
case class BoolLiteral(region: Region, state: Boolean) extends Expression(region) with StageNode with StageVariable
case class HLIdentifier(region: Region, name: String) extends Expression(region) with StageNode
case class StringLiteral(region: Region, name: String) extends Expression(region) with StageNode with StageVariable

case class HLSelf(region: Region) extends Expression(region) with StageNode
case class HLSuper(region: Region) extends Expression(region) with StageNode
case class HLNew(region: Region, tpe: HLType, initList: List[HLInitMember]) extends Expression(region) with StageNode
case class HLInitMember(region: Region, name: String, value: Expression) extends Expression(region) with StageNode

case class HLBranch(
    region: Region,
    condition: Expression,
    caseCheck: Option[HLBranchCaseCheck],
    ifTrue: Expression,
    ifFalse: Option[Expression]
) extends Expression(region)
    with StageNode with StageVariable {}
case class HLBranchCaseCheck(region: Region, tpe: HLType, variables: List[String]) {}
case class HLBinary(region: Region, left: Expression, right: Expression, op: BinaryOP)
    extends Expression(region)
    with StageNode with StageVariable {}
case class Unary(region: Region, value: Expression, op: UnaryOP) extends Expression(region) with StageNode with StageVariable

case class HLClosure(
    region: Region,
    parameters: List[HLClosureParameter],
    returnType: Option[HLType],
    body: Expression
) extends Expression(region)
    with StageNode
case class HLClosureParameter(region: Region, name: String, typeHint: Option[HLType])
case class Return(region: Region, value: Option[Expression]) extends Expression(region) with StageNode with StageVariable

case class Break(region: Region) extends Expression(region) with StageNode with StageVariable
case class HLVarDef(region: Region, name: String, typeHint: Option[HLType], value: Expression)
    extends Expression(region)
    with StageNode

case class VarDef(region: Region, variable: Variable, typeHint: Option[Type], value: Expression)
    extends Expression(region)
    with StageVariable {}
case class While(region: Region, condition: Expression, body: Expression) extends Expression(region) with StageVariable {}

case class For(region: Region, variable: Variable, iterable: Expression, body: Expression)
    extends Expression(region)
    with StageVariable {}

case class Block(region: Region, expressions: List[Expression]) extends Expression(region) with StageVariable {}

case class VariableAccess(region: Region, variable: Variable) extends Expression(region) with StageVariable {}


enum UnaryOP {
    case Negate, Not
}
enum BinaryOP {
    case Add
    case Subtract
    case Multiply
    case Divide
    case Modulo
    case Equal
    case NotEqual
    case GreaterThan
    case LessThan
    case GreaterThanOrEqual
    case LessThanOrEqual
    case And
    case Or
}

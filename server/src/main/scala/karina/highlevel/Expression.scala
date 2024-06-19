package karina.highlevel

import karina.files.ObjectPath
import karina.lexer.Region
import karina.types.*

sealed trait Stage {}

sealed trait StageNode extends Stage {}

sealed trait StageVariable extends Stage {}
sealed trait StageTyped extends Stage {}
sealed abstract class Expression(region: Region) {
    def getRegion: Region = region
}

case class Assign(region: Region, left: Expression, right: Expression)
    extends Expression(region)
    with StageNode
    with StageVariable

case class Array(region: Region, elements: List[Expression])
    extends Expression(region)
    with StageNode
    with StageVariable

case class While(region: Region, condition: Expression, body: Expression)
    extends Expression(region)
    with StageNode
    with StageVariable
    with StageTyped

case class HLFor(region: Region, variable: String, iterable: Expression, body: Expression)
    extends Expression(region)
    with StageNode
case class HLBlock(region: Region, expressions: List[Expression])
    extends Expression(region)
    with StageNode
    with StageVariable

case class FloatLiteral(region: Region, value: Double)
    extends Expression(region)
    with StageNode
    with StageVariable
    with StageTyped
case class IntLiteral(region: Region, value: Long)
    extends Expression(region)
    with StageNode
    with StageVariable
    with StageTyped
case class BoolLiteral(region: Region, state: Boolean)
    extends Expression(region)
    with StageNode
    with StageVariable
    with StageTyped
case class HLName(region: Region, name: String) extends Expression(region) with StageNode
case class StringLiteral(region: Region, name: String)
    extends Expression(region)
    with StageNode
    with StageVariable
    with StageTyped

case class HLSelf(region: Region) extends Expression(region) with StageNode with StageVariable
case class HLSuper(region: Region) extends Expression(region) with StageNode with StageVariable
case class HLNew(region: Region, tpe: Type, initList: List[HLInitMember])
    extends Expression(region)
    with StageNode
    with StageVariable
case class HLInitMember(region: Region, name: String, value: Expression)

case class HLBranch(
    region: Region,
    condition: Expression,
    ifTrue: Expression,
    ifFalse: Option[Expression]
) extends Expression(region)
    with StageNode


enum HLCaseCheck {
    case Name(name: String)
    case Destructor(variables: List[String])
    case None()
}

case class Branch(
    region: Region,
    condition: Expression,
    ifTrue: Expression,
    ifFalse: Option[Expression]
) extends Expression(region)
    with StageVariable


enum CaseCheck {
    case Name(variable: Variable)
    case Destructor(variables: List[Variable])
    case None()
}

case class HLBinary(region: Region, left: Expression, right: Expression, op: BinaryOP)
    extends Expression(region)
    with StageNode
    with StageVariable

case class Unary(region: Region, value: Expression, op: UnaryOP)
    extends Expression(region)
    with StageNode
    with StageVariable

case class HLClosure(
    region: Region,
    parameters: List[HLClosureParameter],
    returnType: Option[Type],
    body: Expression
) extends Expression(region)
    with StageNode

case class HLClosureParameter(region: Region, name: String, typeHint: Option[Type])

case class Closure(
    region: Region,
    parameters: List[ClosureParameter],
    returnType: Option[Type],
    body: Expression,
    captures: List[Variable]
) extends Expression(region)
    with StageVariable

case class ClosureParameter(region: Region, variable: Variable, typeHint: Option[Type])

case class Return(region: Region, value: Option[Expression])
    extends Expression(region)
    with StageNode
    with StageVariable
    with StageTyped

case class Break(region: Region) extends Expression(region) with StageNode with StageVariable
case class HLVarDef(region: Region, name: String, typeHint: Option[Type], value: Expression)
    extends Expression(region)
    with StageNode

case class HLDotName(region: Region, left: Expression, name: String)
    extends Expression(region)
    with StageNode
    with StageVariable
case class HLCall(region: Region, left: Expression, expressions: List[Expression])
    extends Expression(region)
    with StageNode
    with StageVariable
case class HLArrayIndex(region: Region, left: Expression, index: Expression)
    extends Expression(region)
    with StageNode
    with StageVariable
    with StageTyped

case class For(region: Region, variable: Variable, iterable: Expression, body: Expression)
    extends Expression(region)
    with StageVariable

case class StaticFunction(region: Region, path: ObjectPath) extends Expression(region) with StageVariable

case class VariableLink(region: Region, variable: Variable)
    extends Expression(region)
    with StageVariable

case class VarDef(region: Region, variable: Variable, typeHint: Option[Type], value: Expression)
    extends Expression(region)
    with StageVariable
    with StageTyped

//only inner types
case class PackagePath(region: Region, path: ObjectPath) extends Expression(region) with StageVariable
case class UnitPath(region: Region, path: ObjectPath) extends Expression(region) with StageVariable

case class TypedArray(region: Region, tpe: Type, elements: List[Expression]) extends Expression(region) with StageTyped
case class TypedNew(region: Region, tpe: ObjectType, elements: List[Expression])
    extends Expression(region)
    with StageTyped
case class TypedField(region: Region, objType: ObjectType, obj: Expression, fieldType: Type, field: String)
    extends Expression(region)
    with StageTyped

case class TypedBranch(
    region: Region,
    condition: Expression,
    ifTrue: Expression,
    ifFalse: Option[(Type, Expression)]
) extends Expression(region)
    with StageTyped

case class TypedBlock(region: Region, expressions: List[Expression], returnType: Type)
    extends Expression(region)
    with StageTyped

case class CallStaticFunction(region: Region, path: ObjectPath, returnType: Type, expression: List[Expression])
    extends Expression(region)
    with StageTyped

case class CallDynamicFunctionIndirect(region: Region, left: Expression, returnType: Type, expression: List[Expression])
    extends Expression(region)
    with StageTyped

case class CallDynamicFunctionDirect(region: Region, obj: Expression, path: ObjectPath, returnType: Type, expression: List[Expression])
    extends Expression(region)
        with StageTyped

case class TypedStaticFunction(region: Region, path: ObjectPath, inputTypes: List[Type], returnType: Type)
    extends Expression(region)
    with StageTyped

case class TypedFunction(
    region: Region,
    objType: ObjectType,
    obj: Expression,
    name: String,
    path: ObjectPath,
    inputTypes: List[Type],
    returnType: Type
) extends Expression(region)
    with StageTyped

case class TypedBinary(region: Region, left: Expression, right: Expression, op: TypedArithmeticOP)
    extends Expression(region)
    with StageTyped

case class TypedUnary(region: Region, left: Expression, op: TypedUnaryArithmeticOP)
    extends Expression(region)
    with StageTyped

case class IntToFloat(region: Region, value: Expression) extends Expression(region) with StageTyped

case class TypedFor(region: Region, variable: Variable, varType: Type, iterable: Expression, body: Expression)
    extends Expression(region)
    with StageTyped

case class TypedSelf(region: Region, tpe: Type) extends Expression(region) with StageTyped

case class TypedAssignVariable(region: Region, variable: Variable, varType: Type, value: Expression)
    extends Expression(region)
    with StageTyped

case class TypedVariableLink(region: Region, variable: Variable, varType: Type)
    extends Expression(region)
        with StageTyped

case class TypedAssignField(
    region: Region,
    objType: ObjectType,
    obj: Expression,
    fieldType: Type,
    field: String,
    value: Expression
) extends Expression(region)
    with StageTyped

case class TypedAssignArray(region: Region, obj: Expression, index: Expression, value: Expression)
    extends Expression(region)
    with StageTyped

case class TypedClosure(
    region: Region,
    parameters: List[(Variable, Type)],
    returnType: Type,
    body: Expression
) extends Expression(region)
    with StageTyped

case class Variable(region: Region, name: String, location: VariableLocation, isMutable: Boolean)

enum VariableLocation {
    case Local
    case Function
    case Closure
    case CaseCheck
}

enum UnaryOP {
    case Negate, Not
}
enum BinaryOP {
    case Concat
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

enum TypedArithmeticOP {
    case FloatAdd
    case FloatSubtract
    case FloatDivide
    case FloatMultiply
    case FloatGreater
    case FloatGreaterEquals
    case FloatLess
    case FloatLessEquals
    case FloatEquals
    case FloatNotEquals

    case IntAdd
    case IntSubtract
    case IntDivide
    case IntMultiply
    case IntModulo
    case IntGreater
    case IntGreaterEquals
    case IntLess
    case IntLessEquals
    case IntEquals
    case IntNotEquals

    case BooleanEquals
    case BooleanNotEquals
    case BooleanAnd
    case BooleanOr

    def getType(region: Region): Type = {
        this match {
            case FloatAdd | FloatSubtract | FloatDivide | FloatMultiply     => FloatType(region)
            case IntAdd | IntSubtract | IntDivide | IntMultiply | IntModulo => IntegerType(region)
            case FloatGreater | FloatGreaterEquals | FloatLess | FloatLessEquals | FloatEquals | FloatNotEquals =>
                BooleanType(region)
            case IntGreater | IntGreaterEquals | IntLess | IntLessEquals | IntEquals | IntNotEquals =>
                BooleanType(region)
            case BooleanEquals | BooleanNotEquals | BooleanAnd | BooleanOr => BooleanType(region)
        }
    }
}

enum TypedUnaryArithmeticOP {
    case NEGATE_FLOAT
    case NEGATE_INT
    case NOT
}

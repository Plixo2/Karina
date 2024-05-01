package karina.typed

import karina.LanguageException
import karina.highlevel.*


class Variable(name: String) {
    def getName: String = name
}

case class Context(variables: List[Variable]) {
    def addVariable(variable: Variable): Context = {
        Context(variables :+ variable)
    }
    
    def findVariable(name: String): Option[Variable] = {
        variables.find(_.getName == name)
    }
    
    def doesVariableExist(name: String): Boolean = {
        findVariable(name).isDefined
    }
}
class VariableResolver {
    def parse(expression: Expression, context: Context): (Expression with StageVariable, Context) = {
        if (expression.isInstanceOf[StageNode]) {
            throw new LanguageException(expression.getRegion, "Cannot parse stage node")
        }
        expression.asInstanceOf[StageNode] match {
            case HLWhile(region, condition, body) => {
                val (newCondition, _) = parse(condition, context)
                val (newBody, _) = parse(body, context)
                (While(region, newCondition, newBody), context)
            }
            case HLFor(region, variableName, iterable, body) => {
                val (newIterable, _) = parse(iterable, context)
                if (context.doesVariableExist(variableName)) {
                    throw new LanguageException(region, s"Variable $variableName already exists")
                }
                val variable = Variable(variableName)
                val (newBody, _) = parse(body, context.addVariable(variable))
                (For(region, variable, newIterable, newBody), context)
            }
            case HLBlock(region, expressions) => {
                val (newExpressions, _) = expressions.foldLeft((List.empty[Expression with StageVariable], context)) {
                    case ((acc, ctx), expr) => {
                        val (newExpr, newCtx) = parse(expr, ctx)
                        (acc :+ newExpr, newCtx)
                    }
                }
                (Block(region, newExpressions), context)
            }
            case FloatLiteral(region, value) => {
                (FloatLiteral(region, value), context)
            }
            case IntLiteral(region, value) => {
                (IntLiteral(region, value), context)
            }
            case BoolLiteral(region, state) => {
                (BoolLiteral(region, state), context)
            }
            case StringLiteral(region, name) => {
                (StringLiteral(region, name), context)
            }
            case HLIdentifier(region, name) => {
                context.findVariable(name) match {
                    case Some(variable) => (VariableAccess(region, variable), context)
                    case None => throw new LanguageException(region, s"Variable $name does not exist")
                }
                throw new LanguageException(region, "Not implemented")
            }
            case HLSelf(region) => {
                ???
            }
            case HLSuper(region) => ???
            case HLNew(region, tpe, initList) => {
                ???
            }
            case HLInitMember(region, name, value) => ???
            case HLBranch(region, condition, caseCheck, ifTrue, ifFalse) => {
                val (newCondition, _) = parse(condition, context)
                if (caseCheck.isDefined) {
                    throw new LanguageException(region, "Case check is not supported")
                }
                val (newIfTrue, _) = parse(ifTrue, context)
                val newIfFalse = ifFalse.map(parse(_, context)).map(_._1)
                (HLBranch(region, newCondition, None, newIfTrue, newIfFalse), context)
            }
            case HLBinary(region, left, right, op) => {
                val (newLeft, _) = parse(left, context)
                val (newRight, _) = parse(right, context)
                (HLBinary(region, newLeft, newRight, op), context)
            }
            case Unary(region, value, op) => {
                val (newValue, _) = parse(value, context)
                (Unary(region, newValue, op), context)
            }
            case HLClosure(region, parameters, returnType, body) => ???
            case Return(region, value) => {
                val newValue = value.map(parse(_, context)._1)
                (Return(region, newValue), context)
            }
            case Break(region) => {
                (Break(region), context)
            }
            case HLVarDef(region, name, typeHint, value) => {
                val (newValue, _) = parse(value, context)
                val variable = Variable(name)
                (VarDef(region, variable, None, newValue), context.addVariable(variable))
            }
        }
    }
}


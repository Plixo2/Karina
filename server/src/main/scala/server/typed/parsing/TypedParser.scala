package server.typed.parsing

import server.{LanguageException, highlevel}
import server.highlevel.*
import server.typed.*

object TypedParser {

    def parseProgram(list: List[HLStatement]): List[Statement] = {
        val context = new Context()
        val statements = list.map(statement => parseStatement(statement, context))
        statements
    }

    private def parseStatement(statement: HLStatement, context: Context): Statement = {
        statement match {
            case HLLet(name, expression, region) => {
                if (context.containsVariable(name)) {
                    throw new LanguageException(region, s"Variable $name already defined")
                } else {
                    val letVariable = Variable(name)
                    val letStatement = Let(letVariable, parseExpression(expression, context), region)
                    context.pushVariable(letVariable)
                    letStatement
                }
            }
            case HLVariableTerminator(name, region) => {
                context.findVariable(name) match {
                    case Some(variable) => VariableTerminal(variable, region)
                    case None           => throw new LanguageException(region, s"Variable $name not defined")
                }
            }
        }
    }
    private def parseExpression(expression: HLExpression, context: Context): Expression = {
        expression match {
            case HLAddVariables(left, right, region) => {
                val leftVariable = context.findVariable(left) match {
                    case Some(leftVariable) => {
                        leftVariable
                    }
                    case None => throw new LanguageException(region, s"Variable $left not defined")
                }
                val rightVariable = context.findVariable(right) match {
                    case Some(rightVariable) => {
                        rightVariable
                    }
                    case None => throw new LanguageException(region, s"Variable $right not defined")
                }
                Addition(leftVariable, rightVariable, region)
            }
            case HLIntLiteral(value, region) => {
                Integer(value, region)
            }
            case HLVariable(name, region)    => {
                context.findVariable(name) match {
                    case Some(variable) => VariableOther(variable, region)
                    case None           => throw new LanguageException(region, s"Variable $name not defined")
                }
            }
        }
    }
}

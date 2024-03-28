package server.typed.parsing

import server.{LanguageException, highlevel}
import server.highlevel.*
import server.typed.*

object TypedParser {

    def parse_program(list: List[HLStatement]): List[Statement] = {
        val context = new Context()
        val statements = list.map(statement => parse_statement(statement, context))
        statements
    }

    private def parse_statement(statement: HLStatement, context: Context): Statement = {
        statement match {
            case HLLet(name, expression, region) => {
                if (context.contains_variable(name)) {
                    throw new LanguageException(region, s"Variable $name already defined")
                } else {
                    val letVariable = Variable(name)
                    val letStatement = Let(letVariable, parse_expression(expression, context), region)
                    context.push_variable(letVariable)
                    letStatement
                }
            }
            case HLVariableTerminator(name, region) => {
                context.find_variable(name) match {
                    case Some(variable) => VariableTerminal(variable, region)
                    case None           => throw new LanguageException(region, s"Variable $name not defined")
                }
            }
        }
    }
    private def parse_expression(expression: HLExpression, context: Context): Expression = {
        expression match {
            case HLAddVariables(left, right, region) => {
                val leftVariable = context.find_variable(left) match {
                    case Some(leftVariable) => {
                        leftVariable
                    }
                    case None => throw new LanguageException(region, s"Variable $left not defined")
                }
                val rightVariable = context.find_variable(right) match {
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
                context.find_variable(name) match {
                    case Some(variable) => VariableOther(variable, region)
                    case None           => throw new LanguageException(region, s"Variable $name not defined")
                }
            }
        }
    }
}

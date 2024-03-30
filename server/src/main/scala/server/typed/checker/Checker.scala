package server.typed.checker

import server.{LanguageException, typed}
import server.typed.{Addition, Expression, Let, Program, Statement, VariableOther, VariableTerminal}

object Checker {
    def checkProgram(program: Program): Unit = {
        val checker = new CheckContext()
        program.statements.foreach(statement => checkStatement(statement, checker))
        val unusedVariables = checker.getOpenVariables()
        if (unusedVariables.nonEmpty) {
            throw new LanguageException(program.region, s"Variables ${unusedVariables.mkString(", \n")} were not closed")
        }
    }

    private def checkStatement(statement: Statement, context: CheckContext): Unit = {
        statement match {
            case VariableTerminal(variable, region) => {
                if (context.wasClosed(variable)) {
                    throw new LanguageException(region, s"Variable $variable was already closed")
                }
                context.close(variable)
            }
            case Let(variable, value, regionA) => {
                checkExpression(value, context)
                context.insert(variable)
            }
        }
    }

    private def checkExpression(expression: Expression, context: CheckContext): Unit = {
        expression match {
            case Addition(left, right, region) => {
                if (context.wasClosed(left)) {
                    throw new LanguageException(region, s"Variable $left was already closed")
                }
                context.close(left)
                if (context.wasClosed(right)) {
                    throw new LanguageException(region, s"Variable $right was already closed")
                }
                context.close(right)
            }
            case typed.Integer(value, region) => {
                // Nothing to do
            }
            case VariableOther(other, region) => {
                if (context.wasClosed(other)) {
                    throw new LanguageException(region, s"Variable $other was already closed")
                }
                context.close(other)
            }
        }
    }
}

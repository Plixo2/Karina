package server.typed.checker

import server.{LanguageException, typed}
import server.typed.{Addition, Expression, Let, Program, Statement, VariableOther, VariableTerminal}

object Checker {
    def check_program(program: Program): Unit = {
        val checker = new CheckContext()
        program.statements.foreach(statement => check_statement(statement, checker))
        val unusedVariables = checker.getOpenVariables()
        if (unusedVariables.nonEmpty) {
            throw new LanguageException(program.region, s"Variables ${unusedVariables.mkString(", \n")} were not closed")
        }
    }

    private def check_statement(statement: Statement, context: CheckContext): Unit = {
        statement match {
            case VariableTerminal(variable, region) => {
                if (context.was_closed(variable)) {
                    throw new LanguageException(region, s"Variable $variable was already closed")
                }
                context.close(variable)
            }
            case Let(variable, value, regionA) => {
                check_expression(value, context)
                context.insert(variable)
            }
        }
    }

    private def check_expression(expression: Expression, context: CheckContext): Unit = {
        expression match {
            case Addition(left, right, region) => {
                if (context.was_closed(left)) {
                    throw new LanguageException(region, s"Variable $left was already closed")
                }
                context.close(left)
                if (context.was_closed(right)) {
                    throw new LanguageException(region, s"Variable $right was already closed")
                }
                context.close(right)
            }
            case typed.Integer(value, region) => {
                // Nothing to do
            }
            case VariableOther(other, region) => {
                if (context.was_closed(other)) {
                    throw new LanguageException(region, s"Variable $other was already closed")
                }
                context.close(other)
            }
        }
    }
}

package server.interpreter

import server.typed
import server.typed.{Addition, Expression, Let, Program, Variable, VariableOther, VariableTerminal}

import scala.collection.mutable

class Environment(val program: Program) {
    private val statements = program.statements
    private val maxSteps = program.statements.length - 1
    private val bindings = scala.collection.mutable.Map[Variable, Integer]()
    private var pc = 0
    def restart(): Unit = {
        bindings.clear()
        pc = 0;
    }
    
    def evalValue(): Integer = {
        restart();
        while (!has_stopped()) {
            step()
        }
        val last = statements(maxSteps)
        last match {
            case VariableTerminal(variable, regionA) => {
                bindings.get(variable) match {
                    case Some(value) => value
                    case _ => {
                        throw new LanguageRuntimeException(
                            "VariableTerminal must be bound"
                        )
                    }
                }
            }
            case _ => {
                throw new LanguageRuntimeException(
                    "Last statement must be a VariableTerminal"
                )
            }
        }
    }

    def step(): Unit = {
        if (has_stopped()) {
            return
        }
        val statement = statements(pc)
        statement match {
            case VariableTerminal(variable, regionA) => {
                if (pc != maxSteps) {
                    throw new LanguageRuntimeException(
                      "VariableTerminal should be the last statement"
                    )
                }
                if (!bindings.contains(variable)) {
                    throw new LanguageRuntimeException(
                      s"Variable $variable has not been bound"
                    )
                }
                pc += 1
            }
            case Let(variable, value, regionA) => {
                pc += 1
                if (bindings.contains(variable)) {
                    throw new LanguageRuntimeException(
                      s"Variable $variable has already been bound"
                    )
                }
                val result = eval_expression(value)
                bindings(variable) = result
            }
        }
    }

    def has_stopped(): Boolean = {
        pc > maxSteps
    }

    private def eval_expression(expression: Expression): Integer = {
        expression match {
            case Addition(left, right, regionA) => {
                val leftValue = bindings.get(left)
                val rightValue = bindings.get(right)
                (leftValue, rightValue) match {
                    case (Some(leftValue), Some(rightValue)) => {
                        leftValue + rightValue
                    }
                    case _ => {
                        throw new LanguageRuntimeException(
                          "Addition variables must be bound"
                        )
                    }
                }
            }
            case typed.Integer(value, regionA) => {
                value
            }
            case VariableOther(other, regionA) => {
                val value = bindings.get(other)
                value match {
                    case Some(value) => value
                    case None => {
                        throw new LanguageRuntimeException(
                          s"Variable $other has not been bound"
                        )
                    }
                }
            }
        }
    }
}

package server.typed.checker

import server.typed.Variable

class CheckContext {
    private var openVariables: List[Variable] = List[Variable]()
    private var closedVariables: List[Variable] = List[Variable]()

    def insert(variable: Variable): Unit = {
        openVariables = openVariables :+ variable
    }
    def close(variable: Variable): Unit = {
        openVariables = openVariables.filterNot(_ == variable)
        closedVariables = closedVariables :+ variable
    }

    def wasClosed(variable: Variable): Boolean = closedVariables.contains(variable)

    def getOpenVariables(): List[Variable] = openVariables
}

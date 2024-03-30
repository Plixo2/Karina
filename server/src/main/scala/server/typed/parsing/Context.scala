package server.typed.parsing

import server.typed.Variable

class Context {
    private var variables: List[Variable] = List[Variable]()
    
    
    def findVariable(name: String): Option[Variable] = {
        variables.find(_.name == name)
    }
    
    def containsVariable(name: String): Boolean = {
        variables.exists(_.name == name)
    }
    
    def pushVariable(variable: Variable): Unit = {
        variables = variables :+ variable
    }
}

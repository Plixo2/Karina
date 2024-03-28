package server.typed.parsing

import server.typed.Variable

class Context {
    private var variables: List[Variable] = List[Variable]()
    
    
    def find_variable(name: String): Option[Variable] = {
        variables.find(_.name == name)
    }
    
    def contains_variable(name: String): Boolean = {
        variables.exists(_.name == name)
    }
    
    def push_variable(variable: Variable): Unit = {
        variables = variables :+ variable
    }
}

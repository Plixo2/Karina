package server.typed

import server.lexer.Region

sealed abstract class Statement(val region: Region) {}

case class VariableTerminal(variable: Variable, regionA: Region) extends Statement(regionA)
case class Let(variable: Variable, value: Expression, regionA: Region) extends Statement(regionA)

case class Variable(name: String) {

    override def toString: String = s"\"$name\""
}

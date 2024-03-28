package server.typed

import server.lexer.Region

sealed abstract class Expression(var region: Region) {}

case class Addition(left: Variable, right: Variable,regionA: Region) extends Expression(regionA)
case class Integer(value: Int, regionA: Region) extends Expression(regionA)
case class VariableOther(other: Variable, regionA: Region) extends Expression(regionA)

package server.highlevel

import server.lexer.Region

sealed abstract class HLExpression(val region: Region) {}

case class HLAddVariables(left: String, right: String, regionA: Region) extends HLExpression(regionA)
case class HLIntLiteral(value: Int, regionA: Region) extends HLExpression(regionA)
case class HLVariable(name: String, regionA: Region) extends HLExpression(regionA)

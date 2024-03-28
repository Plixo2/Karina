package server.highlevel

import server.lexer.Region

sealed abstract class HLStatement(val region: Region) {}

case class HLVariableTerminator(name: String, regionA: Region) extends HLStatement(regionA)
case class HLLet(name: String, expression: HLExpression, regionA: Region) extends HLStatement(regionA)

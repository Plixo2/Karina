package karina.highlevel

import karina.files.ObjectPath
import karina.lexer.Region

case class HLGenericHint(region: Region, types: List[HLType]) {}

sealed trait HLType {}
case class HLClassType(region: Region, path: ObjectPath, genericHint: Option[HLGenericHint]) extends HLType
case class HLArrayType(region: Region, inner: HLType) extends HLType
case class HLFunctionType(region: Region, parameter: List[HLType], returnType: HLType) extends HLType

case class HLInt() extends HLType
case class HLFloat() extends HLType
case class HLBool() extends HLType
case class HLVoid() extends HLType

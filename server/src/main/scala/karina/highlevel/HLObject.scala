package karina.highlevel

import karina.files.ObjectPath
import karina.lexer.Region

case class HLClass(
    region: Region,
    name: String,
    genericHint: Option[HLGenericDefinition],
    parameter: List[HLParameter],
    superClass: Option[HLClassInheritance],
    fields: List[HLField],
    functions: List[HLFunction]
) {}

case class HLParameter(region: Region, name: String, tpe: HLType) {}

case class HLField(region: Region, name: String, tpe: HLType, value: Expression) {}

case class HLClassInheritance(region: Region, tpe: HLType, memberInits: List[HLInitMember]) {}

case class HLImport(region: Region, path: ObjectPath) {}

case class HLUnit(region: Region, imports: List[HLImport], classes: List[HLClass], functions: List[HLFunction]) {}

case class HLFunction(
    region: Region,
    name: String,
    modifier: Modifier,
    genericHint: Option[HLGenericDefinition],
    parameters: List[HLParameter],
    returnType: HLType,
    expression: Expression
) {}

enum Modifier {
    case Native
    case Override
    case Virtual
    case Normal
}

case class HLGenericDefinition(region: Region, names: List[String]) {}

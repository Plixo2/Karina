package karina.highlevel

import karina.files.ObjectPath
import karina.lexer.Region
import karina.types.Type

case class HLStruct(
    region: Region,
    name: String,
    genericHint: Option[HLGenericDefinition],
    fields: List[HLField],
    functions: List[HLFunction]
) {}

case class HLEnum(
    region: Region,
    name: String,
    genericHint: Option[HLGenericDefinition],
    cases: List[HLEnumCase]
) {}

case class HLEnumCase(region: Region, name: String, types: List[Type]) {}

case class HLParameter(region: Region, name: String, tpe: Type) {}

case class HLField(region: Region, name: String, tpe: Type) {}

case class HLImport(region: Region, path: ObjectPath) {}

case class HLUnit(
    region: Region,
    imports: List[HLImport],
    classes: List[HLStruct],
    enums: List[HLEnum],
    functions: List[HLFunction]
) {}

case class HLPackage(name: String, units: List[(String, HLUnit)], subPackages: List[HLPackage]) {}

case class HLFunction(
    region: Region,
    name: String,
    modifier: Modifier,
    genericHint: Option[HLGenericDefinition],
    parameters: List[HLParameter],
    returnType: Type,
    expression: Expression
) {}

enum Modifier {
    case Native
    case Override
    case Virtual
    case Normal
}

case class HLGenericDefinition(region: Region, names: List[String]) {}

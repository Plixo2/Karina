package karina.highlevel

import karina.files.ObjectPath
import karina.lexer.Region
import karina.types.Type

case class HLClass(
                      region: Region,
                      name: String,
                      genericHint: Option[HLGenericDefinition],
                      parameters: List[HLParameter],
                      superClass: Option[HLClassInheritance],
                      fields: List[HLField],
                      functions: List[HLFunction]
) {}

case class HLParameter(region: Region, name: String, tpe: Type) {}

case class HLField(region: Region, name: String, tpe: Type, value: Expression) {}

case class HLClassInheritance(region: Region, tpe: Type, memberInits: List[Expression]) {}

case class HLImport(region: Region, path: ObjectPath) {}

case class HLUnit(region: Region, imports: List[HLImport], classes: List[HLClass], functions: List[HLFunction]) {}

case class HLPackage(name: String, units: List[(String, HLUnit)], subPackages: List[HLPackage]) {
}

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

package karina.typed

import karina.LanguageException
import karina.files.ObjectPath
import karina.highlevel.*
import karina.lexer.Region
import karina.types.*

import scala.annotation.tailrec

trait ResourceTree {
    def name(): String
    def locate(path: ObjectPath): Option[Resource]

    def allChildren(): List[Resource]

    def locateFunction(path: ObjectPath): Option[DefaultFunction] = {
        locate(path) match {
            case Some(Resource.FunctionResource(function)) => Some(function)
            case _                                         => None
        }
    }
    def locateClass(path: ObjectPath): Option[DefaultClass] = {
        locate(path) match {
            case Some(Resource.ClassResource(clazz)) => Some(clazz)
            case _                                   => None
        }
    }
    def locateUnit(path: ObjectPath): Option[DefaultUnit] = {
        locate(path) match {
            case Some(Resource.CompileUnitResource(unit)) => Some(unit)
            case _                                        => None
        }
    }
    def locatePackage(path: ObjectPath): Option[DefaultPackage] = {
        locate(path) match {
            case Some(Resource.PackageResource(pkg)) => Some(pkg)
            case _                                   => None
        }
    }


}

enum Resource {
    case ClassResource(clazz: DefaultClass)
    case FunctionResource(function: DefaultFunction)
    case FieldResource(field: Field)
    case PackageResource(compilePackage: DefaultPackage)
    case CompileUnitResource(unit: DefaultUnit)
    case ImportResource(imported: Import)
}

case class DefaultPackage(
    packageName: String,
    path: ObjectPath,
    units: List[DefaultUnit],
    subPackages: List[DefaultPackage]
) extends ResourceTree {

    override def name(): String = packageName

    @tailrec
    final override def locate(path: ObjectPath): Option[Resource] = {
        val name = path.head
        units.find(_.name() == name) match {
            case Some(unit) => {
                path.tail match {
                    case Some(tail) => {
                        unit.locate(tail)
                    }
                    case None => {
                        Some(Resource.CompileUnitResource(unit))
                    }
                }
            }
            case None =>
                subPackages.find(_.name() == name) match {
                    case Some(sub) => {
                        path.tail match {
                            case Some(tail) => {
                                sub.locate(tail)
                            }
                            case None => {
                                Some(Resource.PackageResource(sub))
                            }
                        }
                    }
                    case None => None
                }
        }
    }

    override def allChildren(): List[Resource] = {
        units.map(Resource.CompileUnitResource.apply) ++ subPackages.map(Resource.PackageResource.apply)
    }
    def flatUnits(): List[DefaultUnit] = {
        units ++ subPackages.flatMap(_.flatUnits())
        
    }

}

case class DefaultUnit(
    region: Region,
    unitName: String,
    path: ObjectPath,
    allImports: List[Import],
    classes: List[DefaultClass],
    allFunctions: List[DefaultFunction]
) extends ResourceTree {

    override def name(): String = unitName
    override def locate(path: ObjectPath): Option[Resource] = {
        val name = path.head
        classes
            .find(_.name() == name)
            .flatMap(value => {
                path.tail match {
                    case Some(tail) => {
                        value.locate(tail)
                    }
                    case None => {
                        Some(Resource.ClassResource(value))
                    }
                }
            })
            .orElse(
              allFunctions
                  .find(_.name() == name)
                  .flatMap(value => {
                      if (path.tail.isEmpty) {
                          Some(Resource.FunctionResource(value))
                      } else {
                          None
                      }
                  })
            )
    }

    override def allChildren(): List[Resource] = {
        classes.map(Resource.ClassResource.apply) ++
            allFunctions.map(Resource.FunctionResource.apply) ++
            allImports.map(Resource.ImportResource.apply)
    }

    def imports(): List[Import] = {
        allImports
    }

    def functions(): List[DefaultFunction] = {
        allFunctions
    }

}

case class DefaultFunction(
    region: Region,
    isStatic: Boolean,
    functionName: String,
    path: ObjectPath,
    modifier: Modifier,
    returnType: Type,
    parameters: List[Parameter],
    definedGenerics: List[GenericType],
    body: Expression
) {
    def name(): String = functionName

    override def toString: String = {
        val params = parameters.map(p => s"${p.name}: ${p.tpe}").mkString(", ")
        s"fn ${modifier match
                case Modifier.Native   => "N "
                case Modifier.Override => "O "
                case Modifier.Virtual  => "V "
                case Modifier.Normal   => ""
            }${definedGenerics.map(_.toString).mkString("<", ", ", ">")} $functionName($params): $returnType = ..."
    }


}

case class DefaultClass(
    region: Region,
    className: String,
    path: ObjectPath,
    genericDefinition: List[GenericType],
    parameters: List[Parameter],
    fields: List[Field],
    functions: List[DefaultFunction],
    inheritance: Option[Inheritance]
) extends ResourceTree {
    override def name(): String = className
    override def locate(path: ObjectPath): Option[Resource] = {
        val name = path.head
        fields.find(_.name == name) match {
            case Some(value) => {
                if (path.tail.isEmpty) {
                    Some(Resource.FieldResource(value))
                } else {
                    None
                }
            }
            case None => {
                functions
                    .find(_.name() == name)
                    .flatMap(value => {
                        if (path.tail.isEmpty) {
                            Some(Resource.FunctionResource(value))
                        } else {
                            None
                        }
                    })
            }
        }
    }

    def definedGeneric(): List[String] = genericDefinition.map(_.name)

    override def allChildren(): List[Resource] = {
        fields.map(Resource.FieldResource.apply) ++ functions.map(Resource.FunctionResource.apply)
    }
}
case class Parameter(region: Region, name: String, tpe: Type, variableObject: Option[Variable])

case class Inheritance(region: Region, tpe: Type, initList: List[Expression])
case class MemberInit(name: String, expression: Expression)

case class Import(region: Region, path: ObjectPath) {}

case class Field(region: Region, name: String, tpe: Type, expression: Expression) {
    override def toString: String = s"$name: $tpe = ..."
}

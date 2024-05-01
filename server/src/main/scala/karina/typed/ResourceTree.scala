package karina.typed

import karina.files.ObjectPath
import karina.highlevel.*
import karina.types.{GenericType, Type}

import scala.annotation.tailrec

trait ResourceTree {
    def name(): String
    def locate(path: ObjectPath): Option[Resource]
    def locateFunction(path: ObjectPath): Option[Function] = {
        locate(path) match {
            case Some(Resource.FunctionResource(function)) => Some(function)
            case _                                         => None
        }
    }
    def locateClass(path: ObjectPath): Option[Class] = {
        locate(path) match {
            case Some(Resource.ClassResource(clazz)) => Some(clazz)
            case _                                   => None
        }
    }
    def locateUnit(path: ObjectPath): Option[CompileUnit] = {
        locate(path) match {
            case Some(Resource.CompileUnitResource(unit)) => Some(unit)
            case _                                        => None
        }
    }

    override def toString: String = {
        val buffer = new StringBuilder(50)
        print(buffer, "", "")
        s"\n $buffer"
    }

    private def print(buffer: StringBuilder, prefix: String, childrenPrefix: String): Unit = {
        buffer.append(prefix)
        buffer.append(name)
        buffer.append(" ")
        if (isLiteral) {
            buffer.append("\"").append(tokenRecord.literal).append("\"")
        }
        buffer.append('\n')
        val it = children.iterator
        while (it.hasNext) {
            val next: Node = it.next
            if (it.hasNext) next.print(buffer, childrenPrefix + "├── ", childrenPrefix + "│   ")
            else next.print(buffer, childrenPrefix + "└── ", childrenPrefix + "    ")
        }
    }
}

enum Resource {
    case ClassResource(clazz: Class)
    case FunctionResource(function: Function)
    case FieldResource(field: Field)
    case PackageResource(compilePackage: CompilePackage)
    case CompileUnitResource(unit: CompileUnit)
}

case class CompilePackage(packageName: String, units: List[CompileUnit], subPackages: List[CompilePackage])
    extends ResourceTree {

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
}

sealed trait CompileUnit extends ResourceTree {}

case class NodeUnit(unitName: String, hlUnit: HLUnit) extends CompileUnit {
    override def name(): String = unitName
    override def locate(path: ObjectPath): Option[Resource] = {
        val name = path.head
        hlUnit.classes
            .find(_.name == name)
            .flatMap(value => {
                val nodeClass = NodeClass(value)
                path.tail match {
                    case Some(tail) => {
                        nodeClass.locate(tail)
                    }
                    case None => {
                        Some(Resource.ClassResource(nodeClass))
                    }
                }
            })
    }
}

case class TypedUnit(unitName: String, classes: List[Class], imports: ObjectPath, functions: List[Function])
    extends ResourceTree
    with CompileUnit {

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
              functions
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

}

sealed trait Function {
    def name(): String
}
case class NodeFunction(hlFunction: HLFunction) extends Function {
    override def name(): String = hlFunction.name
}

case class SignatureOnlyFunction(functionName: String, tpe: Type, parameters: List[(String, Type)], body: Expression)
    extends Function {
    override def name(): String = functionName
}

case class TypedFunction(functionName: String, tpe: Type, parameters: List[(String, Type)], body: Expression)
    extends Function {
    override def name(): String = functionName
}

sealed trait Class extends ResourceTree {
    def definedGeneric(): List[String]
}
case class NodeClass(hlClass: HLClass) extends Class {
    override def name(): String = hlClass.name
    override def locate(path: ObjectPath): Option[Resource] = {
        None
    }
    override def definedGeneric(): List[String] = hlClass.genericHint.map(_.names).getOrElse(List())
}

case class ClassWithOutInheritance(
    className: String,
    genericDefinition: List[GenericType],
    parameters: List[(String, Type)],
    fields: List[Field],
    functions: List[Function],
    unresolvedInheritance: Option[HLClassInheritance]
) extends Class {
    override def name(): String = className
    override def locate(path: ObjectPath): Option[Resource] = {
//        val name = path.head
//        fields.find(_.name == name) match {
//            case Some(value) => {
//                if (path.tail.isEmpty) {
//                    Some(Resource.FieldResource(value))
//                } else {
//                    None
//                }
//            }
//            case None => {
//                if (functions.name() == name) {
//                    if (path.tail.isEmpty) {
//                        Some(Resource.FunctionResource(functions))
//                    } else {
//                        None
//                    }
//                } else {
//                    None
//                }
//            }
        None
    }

    override def definedGeneric(): List[String] = genericDefinition.map(_.name)
}

case class TypedClass(
    className: String,
    genericDefinition: List[String],
    parameters: List[(String, Type)],
    fields: List[Field],
    function: List[Function]
) extends ResourceTree
    with Class {

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
                function
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

    override def definedGeneric(): List[String] = genericDefinition
}

case class Field(name: String, tpe: Type, expression: Expression) {}

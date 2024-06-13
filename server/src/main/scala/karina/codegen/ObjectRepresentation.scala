package karina.codegen

import karina.LanguageException
import karina.typed.{DefaultClass, DefaultPackage, expectWithRegion}
import karina.types.ObjectType

object ObjectRepresentation {

    def load(root: DefaultPackage, tpe: ObjectType): MemoryObject = {
        val defaultClass = root.locateClass(tpe.path).expectWithRegion(tpe.region, "Class not found")
        val startSlots = List[String]()
        MemoryObject(defaultClass.path.mkString("."), loadSlots(root, tpe, startSlots))
    }

    def loadFromObj(root: DefaultPackage, defaultClass: DefaultClass): MemoryObject = {
        def loadSlots_(): List[String] = {
            var slots = List[String]()
            defaultClass.inheritance match {
                case Some(value) => {
                    value.tpe match {
                        case objectType: ObjectType =>
                            loadSlots(root, objectType, slots)
                        case _ => {
                            throw new LanguageException(defaultClass.region, "Inheritance cant be represented")
                        }
                    }
                }
                case None => {
                }
            }
            defaultClass.fields.foreach { ref =>
                slots = slots :+ ref.name
            }
            defaultClass.parameters.foreach { ref =>
                slots = slots :+ ref.name
            }
            defaultClass.functions.foreach { ref =>
                slots = slots :+ ref.name()
            }
            slots
        }
        MemoryObject(defaultClass.path.mkString("."), loadSlots_())
    }

    private def loadSlots(root: DefaultPackage, tpe: ObjectType, prev: List[String]): List[String] = {
        val defaultClass = root.locateClass(tpe.path).expectWithRegion(tpe.region, "Class not found")
        var slots = prev
        defaultClass.inheritance match {
            case Some(value) => {
                value.tpe match {
                    case objectType: ObjectType =>
                        loadSlots(root, objectType, slots)
                    case _ => {
                        throw new LanguageException(tpe.region, "Inheritance cant be represented")
                    }
                }
            }
            case None => {
            }
        }
        defaultClass.fields.foreach { ref =>
            slots = slots :+ ref.name
        }
        defaultClass.parameters.foreach { ref =>
            slots = slots :+ ref.name
        }
        defaultClass.functions.foreach { ref =>
            slots = slots :+ ref.name()
        }
        slots
    }

    case class MemoryObject(name: String,slots: List[String]) {
        def size(): Int = slots.size

        def getSlot(name: String): Int = {
            slots.indexOf(name)
        }
    }
}

package karina.codegen

import karina.LanguageException
import karina.typed.{DefaultClass, DefaultPackage, expectWithRegion}
import karina.types.ObjectType

object ObjectRepresentation {

    def load(root: DefaultPackage, tpe: ObjectType): MemoryObject = {
        val defaultClass = root.locateClass(tpe.path).expectWithRegion(tpe.region, "Class not found")
        loadFromObj(defaultClass)
    }

    def loadFromObj(defaultClass: DefaultClass): MemoryObject = {
        var startSlots = List[String]()
        defaultClass.fields.foreach { ref =>
            startSlots = startSlots :+ ref.name
        }
        defaultClass.functions.foreach { ref =>
            startSlots = startSlots :+ ref.name()
        }
        val str = defaultClass.fields.map(ref => ref.tpe.getLowLevelType()).mkString(", ")

        MemoryObject(defaultClass.path.mkString("."), startSlots, str)
    }
    

    case class MemoryObject(name: String,slots: List[String], gcMask: String) {
        def size(): Int = slots.size

        def getSlot(name: String): Int = {
            slots.indexOf(name)
        }
    }
}

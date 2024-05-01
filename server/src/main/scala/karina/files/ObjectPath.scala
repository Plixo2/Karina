package karina.files

import scala.annotation.targetName
import scala.util.matching.Regex

class ObjectPath(val elements: List[String]) {
    if (elements.isEmpty) {
        throw new IllegalArgumentException("ObjectPath cannot be empty")
    }
    def this(element: String, split: String) = {
        this(element.split(Regex.quote(split)).toList)
    }

    def head: String = {
        elements.head
    }
    def tail: Option[ObjectPath] = {
        if (elements.length == 1) {
            None
        } else {
            Some(new ObjectPath(elements.tail))
        }
    }
    
    def length: Int = {
        elements.length
    }
    
    def add(name: String): ObjectPath = {
        new ObjectPath(elements :+ name)
    }

    def join(other: ObjectPath): ObjectPath = {
        new ObjectPath(this.elements ++ other.elements)
    }

    override def toString: String = {
        s"${elements.mkString(" ")} [${elements.length}]"
    }
}

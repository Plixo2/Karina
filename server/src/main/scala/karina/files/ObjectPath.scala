package karina.files

import scala.annotation.targetName
import scala.util.matching.Regex

class ObjectPath(val elements: List[String], allowEmpty: Boolean = false) {
    if (elements.isEmpty && !allowEmpty) {
        throw new IllegalArgumentException("ObjectPath cannot be empty")
    }
    def this(element: String, split: String, allowEmpty: Boolean) = {
        this(element.split(Regex.quote(split)).toList, allowEmpty)
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
        s"${elements.mkString("-")}"
    }
    def mkString(separator: String): String = {
        elements.mkString(separator)
    }

    override def equals(obj: Any): Boolean = {
        obj match {
            case other: ObjectPath => elements.equals(other.elements)
            case _                 => false
        }
    }
}

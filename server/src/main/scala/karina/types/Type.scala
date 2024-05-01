package karina.types

import karina.files.ObjectPath

sealed trait Type {}

case class GenericType(name: String, source: GenericSource) extends Type {}

enum GenericSource {
    case Class(path: ObjectPath)
    case Function(path: ObjectPath)
}

case class ArrayType(t: Type) extends Type {}
case class FloatType() extends Type {}
case class VoidType() extends Type {}
case class IntegerType() extends Type {}
case class StringType() extends Type {}
case class BooleanType() extends Type {}

case class ObjectType(path: ObjectPath, binds: Map[String, Type]) extends Type {
    def bind(name: String, t: Type): ObjectType = {
        ObjectType(path, binds + (name -> t))
    }
}

package karina.types

import karina.LanguageException
import karina.files.ObjectPath
import karina.highlevel.TypedStaticFunction
import karina.lexer.Region
import karina.typed.{DefaultFunction, DefaultPackage, expectWithRegion}

sealed trait Type {
    def getRegion(): Region
    
    def getLowLevelType(): String = {
        this match {
            case FloatType(_)                 => "f"
            case VoidType(_)                  => "v"
            case IntegerType(_)               => "i"
            case BooleanType(_)               => "i"
            case _ => "p"
        }
    }
}

case class ObjectTypeDefault(region: Region, path: ObjectPath, generics: List[Type]) extends Type {
    override def toString: String = {
        s"$path<${generics.mkString(", ")}>"
    }
    override def getRegion(): Region = region
}

case class GenericType(region: Region, name: String, source: GenericSource) extends Type {
    override def toString: String = {
        s"<$name>"
    }
    override def getRegion(): Region = region
}

case class FunctionType(region: Region, args: List[Type], ret: Type) extends Type {
    override def toString: String = {
        s"(${args.mkString(", ")}) -> $ret"
    }
    override def getRegion(): Region = region
}

enum GenericSource {
    case Class(path: ObjectPath)
    case Function(path: ObjectPath)
    
    def equals(other: GenericSource): Boolean = {
        this match {
            case Class(path) => other match {
                case Class(path2) => path.equals(path2)
                case _ => false
            }
            case Function(path) => other match {
                case Function(path2) => path.equals(path2)
                case _ => false
            }
        }
    }
}

case class ArrayType(region: Region, t: Type) extends Type {
    override def toString: String = {
        s"[$t]"
    }
    override def getRegion(): Region = region
}
case class FloatType(region: Region) extends Type {
    override def toString: String = {
        "Float"
    }
    override def getRegion(): Region = region
}
case class VoidType(region: Region) extends Type {
    override def toString: String = {
        "Void"
    }
    override def getRegion(): Region = region
}
case class IntegerType(region: Region) extends Type {
    override def toString: String = {
        "Int"
    }
    override def getRegion(): Region = region
}
case class StringType(region: Region) extends Type {
    override def toString: String = {
        "String"
    }
    override def getRegion(): Region = region
}
case class BooleanType(region: Region) extends Type {
    override def toString: String = {
        "Bool"
    }
    override def getRegion(): Region = region
}

case class BaseType(region: Region) extends Type {
    override def getRegion(): Region = region
}

class Resolvable(region: Region) extends Type {
    private val hash: Int = this.hashCode()
    private var resolved: Option[Type] = None

    def resolve(tpe: Type): Unit = {
        if (tpe == this) {
            throw new LanguageException(region, "Cannot resolve to itself")
        }
        if (resolved.isDefined) {
            throw new Exception("Already resolved")
        }
        resolved = Some(tpe)
    }

    def get(): Type = {
        if (resolved.isEmpty) {
            throw new Exception("Not resolved")
        }
//        val resolvedType = resolved.get
//        resolvedType match {
//            case r: Resolvable => if (r.isResolved) r.get() else resolvedType
//            case _             => resolvedType
//        }
        resolved.get
    }

    def isResolved: Boolean = resolved.isDefined

    override def toString: String = {
        s"#$hash (${resolved.map(_.toString).getOrElse("unresolved")}) "
    }
    override def getRegion(): Region = region
}

case class ObjectType(region: Region, path: ObjectPath, binds: Map[GenericType, Type]) extends Type {

    def getTypeReplaced(tpe: Type): Type = {
        getReplacedType(tpe, binds)
    }
    

    def getField(root: DefaultPackage, name: String): Option[Type] = {
        val classes = root.locateClass(path).expectWithRegion(region, "Getting path")
        classes.fields.find(_.name == name).map(ref => getTypeReplaced(ref.tpe))
    }

    def getFunction(root: DefaultPackage, name: String): Option[(FunctionType, ObjectPath)] = {
        val classes = root.locateClass(path).expectWithRegion(region, "Getting path")

        classes.functions
            .find(ref => ref.name() == name)
            .map(ref => {
                val binds = ref.definedGenerics.map(ref => (ref, Resolvable(ref.region))).toMap
                val inputTypes = ref.parameters.map(ref => getTypeReplaced(ref.tpe))
                val boundInputs = inputTypes.map(ref => getReplacedType(ref, binds))
                val returnType = getReplacedType(getTypeReplaced(ref.returnType), binds)
                (FunctionType(ref.region, boundInputs, returnType), ref.path)
            })
    }

    override def toString: String = {
        s"${path.toString}${
                if (binds.nonEmpty) binds.map { case (k, v) => s"$k -> $v" }.mkString("{", ", ", "}") else ""
            }"
    }
    override def getRegion(): Region = region

}

def getReplacedType(tpe: Type, binds: Map[GenericType, Type]): Type = {
    tpe match {
        case ObjectTypeDefault(region, path, generics) =>
            throw new LanguageException(region, "Cannot replace generics in default object type")
        case gen: GenericType => {
            if (binds.contains(gen)) {
                binds(gen)
            } else {
                gen
            }
        }
        case FunctionType(region, args, ret) =>
            FunctionType(region, args.map(ref => getReplacedType(ref, binds)), getReplacedType(ret, binds))
        case ArrayType(region, t)            => ArrayType(region, getReplacedType(t, binds))
        case FloatType(region)               => FloatType(region)
        case VoidType(region)                => VoidType(region)
        case IntegerType(region)             => IntegerType(region)
        case StringType(region)              => StringType(region)
        case BooleanType(region)             => BooleanType(region)
        case resolvable: Resolvable          => resolvable
        case ObjectType(region, path, sbinds) => {
            val replaced = sbinds.map(ref => (ref._1, getReplacedType(ref._2, binds)))
            ObjectType(region, path, replaced)
        }
        case BaseType(region) => BaseType(region)
    }
}

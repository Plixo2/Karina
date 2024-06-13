package karina.parser

import karina.LanguageException
import karina.lexer.{Region, TokenRecord}

case class Node(name: String, region: Region, children: List[Node], tokenRecord: TokenRecord, isLiteral: Boolean) {

    def get(name: String): Option[Node] = {
        children.find(_.name.toLowerCase == name.toLowerCase) match {
            case Some(value) => Some(value)
            case None        => None
        }
    }

    def apply(name: String): Node = {
        children.find(_.name == name.toLowerCase) match {
            case Some(value) => value
            case None => {
                throw new LanguageException(region, s"Expected $name on Node '${this.name}'")
            }
        }
    }

    def has(name: String): Boolean = {
        children.exists(_.name == name.toLowerCase)
    }

    def id(): String = {
        if (name.toLowerCase == "id") {
            get("word").expect("Expected word on rule 'id'").tokenRecord.literal
        } else {
            get("id").expect(s"token literal called id on ${this.name}").id()
        }
    }

    def number(): String = {
        if (name.toLowerCase == "number") {
            get("number").expect("Expected number on rule 'number'").tokenRecord.literal
        } else {
            get("number").expect(s"token literal called number on ${this.name}").number()
        }
    }

    def string(): String = {
        if (name.toLowerCase == "stringliteral") {
            get("stringliteral").expect("Expected string on rule 'stringliteral'").tokenRecord.literal
        } else {
            get("stringliteral").expect(s"Token literal called 'stringliteral' on ${this.name}").string()
        }
    }

    def map[T](name: String, f: Node => T): List[T] = {
        children.filter(_.name == name.toLowerCase).map(f)
    }

    def find(predicate: Node => Boolean): Option[Node] = {
        children.find(predicate)
    }

    def getAll(name: String): List[Node] = {
        children.filter(_.name == name.toLowerCase)
    }

    def assertType(name: String): Unit = {
        assert(
          name.toLowerCase == this.name.toLowerCase,
          s"Expected ${name.toLowerCase}, found ${this.name.toLowerCase}"
        )
    }

    def error(msg: String): LanguageException = {
        new LanguageException(this.region, msg)
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

enum NodeResult {
    case Some(node: Node)
    case None(region: Region)

    def expect(msg: String): Node = {
        this match {
            case NodeResult.Some(node)   => node
            case NodeResult.None(region) => throw new LanguageException(region, "Expected " + msg)
        }
    }

    def map[T](function: Node => T): Option[T] = {
        this match {
            case NodeResult.Some(node) => Option.apply(function(node))
            case NodeResult.None(_)    => Option.empty
        }
    }

    def asOption(): Option[Node] = {
        map(identity)
    }

    def isDefined: Boolean = {
        this match {
            case NodeResult.Some(_) => true
            case NodeResult.None(_) => false
        }
    }

    def get(): Node = {
        this match {
            case NodeResult.Some(node)   => node
            case NodeResult.None(region) => throw new LanguageException(region, "Expected node")
        }
    }
}

import scala.util.chaining.*

extension [T](option: Option[T]) {
    def expect(msg: String): T = option.getOrElse {
        throw new IllegalStateException(s"Expected $msg")
    }
}

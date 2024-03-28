package server.parser

import server.LanguageException
import server.lexer.{StringToken, TokenRecord, WordToken}

case class ModifiableTokenStream(private val list: List[TokenRecord]) {
    private var position = 0

    def current(): TokenRecord = {
        if (position >= list.length) {
            if (list.isEmpty) throw new IllegalStateException("No more tokens left")

            val tokenRecord = currentOrLast()
            throw new LanguageException(tokenRecord.region, "No more tokens left")
        }
        list(position)
    }

    def consume(): Unit = position += 1

    def hasLeft: Boolean = position < list.length

    def index(): Int = position

    def apply(int: Int): TokenRecord = list(int)

    def reset(index: Int): Unit = position = index

    def tokenLeft(): List[TokenRecord] = {
        var list = List[TokenRecord]()
        while (hasLeft) {
            list = list :+ current()
            this.consume()
        }
        list
    }

    def currentOrLast(): TokenRecord = if (hasLeft) current() else list.last

    def expectWord(): String = {
        val record = current()
        assert(record.token == WordToken, s"Expected a word, but got ${record.literal}")
        record.literal
    }

    def expectLiteral(element: String): Unit = {
        val record = current()
        assert(record.literal == element, s"Expected $element, but got ${record.literal}")
    }

    def testLiteral(element: String): Boolean = {
        if (!hasLeft) return false
        val record = current()
        record.literal == element
    }

    def isWord: Boolean = {
        hasLeft && current().token == WordToken
    }

    def isString: Boolean = hasLeft && current().token == StringToken

}

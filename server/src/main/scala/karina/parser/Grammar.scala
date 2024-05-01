package karina.parser

import karina.LanguageException
import karina.files.InternalFile
import karina.lexer.*

final class Grammar(file: InternalFile) {
    private var solveLater = List[RuleSet => Unit]()
    def parse(languageTokens: List[Token]): RuleSet = {
        solveLater = List()
        val tokens = this.tokens()
        val tokenizer = Tokenizer(tokens)
        var rules = List[Rule]()
        for ((line, index) <- file.lines().zipWithIndex) {
            val tokens = tokenizer.tokenizeLine(file, line, index)
            if (tokens.nonEmpty) {
                val rule = parseRule(ModifiableTokenStream(tokens), languageTokens)
                if (rules.exists(foundRule => foundRule.name.equalsIgnoreCase(rule.name))) {
                    throw new LanguageException(tokens.head.region, s"Rule ${rule.name.toLowerCase} already defined")
                }
                rules = rules :+ rule
            }
        }
        val ruleSet = RuleSet(rules)
        solveLater.foreach(_(ruleSet))
        solveLater = List()
        ruleSet
    }

    private def parseRule(stream: ModifiableTokenStream, tokens: List[Token]): Rule = {
        val name = stream.expectWord()
        stream.consume()
        stream.expectLiteral("::=")
        stream.consume()
        val expression = parseExpression(stream, tokens)
        Rule(name.toLowerCase, expression)
    }

    private def parseExpression(stream: ModifiableTokenStream, tokens: List[Token]): GrammarElement = {
        val options = parseOptions(stream, tokens);

        if (options.size == 1) {
            options.head
        } else {
            if (options.isEmpty) {
                val region = stream.currentOrLast().region
                OptionsElement(region, List())
            } else {
                val region = options.head.region.setRight(options.last.region.to)
                OptionsElement(region, options)
            }
        }

    }

    private def parseOptions(stream: ModifiableTokenStream, tokens: List[Token]): List[GrammarElement] = {
        var options = List[GrammarElement]()
        var matchedOption = true
        while (matchedOption) {
            matchedOption = false
            val innerList = parseSequence(stream, tokens)
            if (innerList.size == 1) {
                options = options :+ innerList.head
            } else {
                if (innerList.nonEmpty) {
                    val endRegion = innerList.head.region.setRight(innerList.last.region.to)
                    options = options :+ SequenceElement(endRegion, innerList)
                }
            }
            if (stream.testLiteral("|")) {
                stream.consume()
                matchedOption = true
            }
        }
        options
    }

    private def parseSequence(stream: ModifiableTokenStream, tokens: List[Token]): List[GrammarElement] = {
        var matchedList = true;
        var innerList = List[GrammarElement]()

        while (matchedList) {
            if (stream.isString) {
                val tokenRecord = stream.current()
                val stringedLiteral = tokenRecord.literal
                assert(stringedLiteral.length >= 2)
                val literalName = stringedLiteral.substring(1, stringedLiteral.length - 1)
                stream.consume()
                val foundToken = tokens.find(token => token.alias == literalName)
                foundToken match {
                    case Some(value) => {
                        val literal = Literal(tokenRecord.region, value)
                        innerList = innerList :+ applyPostfix(stream, literal)
                    }
                    case None =>
                        throw new LanguageException(tokenRecord.region, s"Token '$literalName' not found")
                }
            } else if (stream.isWord) {
                val tokenRecord = stream.current()
                val word = stream.expectWord()
                stream.consume()
                val rule = RuleEntry(tokenRecord.region, null)
                solveLater = solveLater :+ (ruleSet => {
                    val found = ruleSet.findRule(word)
                    found match {
                        case Some(value) => rule.rule = value
                        case None        => throw new LanguageException(tokenRecord.region, s"Rule $word not found")
                    }
                })
                innerList = innerList :+ applyPostfix(stream, rule)
            } else if (stream.testLiteral("(")) {
                stream.consume()
                val subExpression = parseExpression(stream, tokens)
                stream.expectLiteral(")")
                stream.consume()
                innerList = innerList :+ applyPostfix(stream, subExpression)
            } else {
                matchedList = false;
            }
        }
        innerList
    }

    private def applyPostfix(stream: ModifiableTokenStream, element: GrammarElement): GrammarElement = {
        if (stream.hasLeft) {
            val region = stream.current().region
            if (stream.testLiteral("!")) {
                stream.consume()
                NecessaryElement(region, element)
            } else if (stream.testLiteral("?")) {
                stream.consume()
                MultiElement(region, Multiplicity.ZeroOrOne, element)
            } else if (stream.testLiteral("*")) {
                stream.consume()
                MultiElement(region, Multiplicity.ZeroOrMore, element)
            } else {
                element
            }
        } else {
            element
        }

    }

    private def tokens(): List[Token] = {
        List(
          WhiteSpaceToken,
          StringToken,
          WordToken,
          CommentToken,
          CharToken('!'),
          CharToken('*'),
          CharToken('?'),
          CharToken('('),
          CharToken(')'),
          CharToken('|'),
          LiteralToken("::=")
        )
    }
}

sealed trait GrammarElement(val region: Region) {}

class Rule(val name: String, var expression: GrammarElement) {
    override def toString: String = {
        s"Rule($name)"
    }
}

case class Literal(override val region: Region, token: Token) extends GrammarElement(region)

case class RuleEntry(override val region: Region, var rule: Rule) extends GrammarElement(region)

case class SequenceElement(override val region: Region, expressions: List[GrammarElement])
    extends GrammarElement(region)

case class OptionsElement(override val region: Region, elements: List[GrammarElement]) extends GrammarElement(region)

case class MultiElement(override val region: Region, count: Multiplicity, element: GrammarElement)
    extends GrammarElement(region)

case class NecessaryElement(override val region: Region, element: GrammarElement) extends GrammarElement(region)

enum Multiplicity {
    case ZeroOrOne
    case ZeroOrMore
}

case class RuleSet(private val rules: List[Rule]) {
    def findRule(name: String): Option[Rule] = {
        rules.find(rule => rule.name.equalsIgnoreCase(name))
    }

    def getRules: List[Rule] = List.from(rules)
}

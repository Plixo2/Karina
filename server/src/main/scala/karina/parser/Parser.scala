package karina.parser

import karina.LanguageException
import karina.lexer.TokenRecord

import scala.annotation.tailrec
final class Parser(private val entryRule: Rule) {

    def parse(records: List[TokenRecord]): Option[Node] = {
        val stream = ModifiableTokenStream(records)
        val result = testRule(entryRule, stream)
        result match {
            case Some(value) => {
                if (stream.hasLeft) {
                    val lastMatch = stream.current()
                    println(s"tokens left ${stream.tokenLeft()}")
                    throw new LanguageException(
                      lastMatch.region,
                      s"Failure of Rule '$entryRule' \n${lastMatch.region.prettyString()}\nin Source"
                    )
                } else {
                    Some(value)
                }
            }
            case None => {
                None
            }
        }
    }

    private def testRule(rule: Rule, stream: ModifiableTokenStream): Option[Node] = {
        if (!stream.hasLeft) {
            return None
        }
        val startRegion = stream.current().region
        val result = testExpression(rule, rule.expression, stream)
        var currentOrLast = stream.currentOrLast()
        stream.get(stream.index() - 1) match {
            case Some(value) => {
                currentOrLast = value
            }
            case _ => {}
        }
        val endPosition = currentOrLast.region.to
        val region = startRegion.setRight(endPosition)
        result.map(elements => Node(rule.name, region, elements, currentOrLast, false))
    }

    private def testExpression(
        owningRule: Rule,
        expression: GrammarElement,
        stream: ModifiableTokenStream
    ): SyntaxResult = {
        // <editor-fold desc="Parsing">

        def parseLiteral(tokenElement: Literal): SyntaxResult = {
            if (!stream.hasLeft) {
                None
            } else {
                val record = stream.current()
                if (record.token == tokenElement.token) {
                    stream.consume()
                    Some(List(Node(tokenElement.token.alias, record.region, List(), record, true)))
                } else {
                    None
                }
            }
        }

        def parseSequenceElement(sequenceElement: SequenceElement): SyntaxResult = {
            def testElements(elements: List[GrammarElement]): SyntaxResult = {
                elements match {
                    case Nil => Some(List())
                    case head :: tail => {
                        val subExpr = testExpression(owningRule, head, stream)
                        subExpr.flatMap(elements => {
                            testElements(tail).map(otherElements => elements ++ otherElements)
                        })
                    }
                }
            }

            testElements(sequenceElement.expressions)
        }
        def parseOptionElement(optionsElement: OptionsElement): SyntaxResult = {
            @tailrec
            def testElements(elements: List[GrammarElement]): SyntaxResult = {
                elements match {
                    case Nil => None
                    case head :: tail => {
                        val tokenPosition = stream.index()
                        val subExpr = testExpression(owningRule, head, stream)
                        subExpr match {
                            case Some(elements) => Some(elements)
                            case None => {
                                stream.reset(tokenPosition)
                                testElements(tail)
                            }
                        }
                    }
                }
            }

            testElements(optionsElement.elements)
        }
        def parseNecessaryElement(necessaryElement: NecessaryElement): SyntaxResult = {
            val syntaxResult = testExpression(owningRule, necessaryElement.element, stream)
            val currentOrLastRecord = stream.currentOrLast()
            syntaxResult match {
                case Some(elements) => syntaxResult
                case None => {
                    throw new LanguageException(
                      currentOrLastRecord.region,
                      s"Failure of Element ${necessaryElement.element} in Rule '$owningRule' \n${necessaryElement.element.region.prettyString()}\nin Source"
                    )
                }
            }
        }
        def parseMultiElement(multiElement: MultiElement): SyntaxResult = {
            multiElement.count match {
                case Multiplicity.ZeroOrOne => {
                    val pos = stream.index()
                    stream.reset(pos)
                    val subResult = testExpression(owningRule, multiElement.element, stream)
                    subResult match {
                        case Some(elements) => Some(elements)
                        case None => {
                            stream.reset(pos)
                            Some(List())
                        }
                    }
                }
                case Multiplicity.ZeroOrMore => {
                    var nodes = List[Node]()
                    var latest: SyntaxResult = Some(List[Node]());
                    // that's a do while loop
                    while ({
                        val pos = stream.index()
                        latest = testExpression(owningRule, multiElement.element, stream)
                        latest match {
                            case Some(elements) => {
                                nodes = nodes ++ elements
                                true
                            }
                            case None => {
                                stream.reset(pos)
                                false
                            }
                        }
                    }) ()
                    latest match {
                        case Some(elements) => Some(nodes)
                        case None           => Some(nodes)
                    }
                }
            }
        }
        // </editor-fold>
        expression match {
            case literal: Literal => {
                parseLiteral(literal)
            }
            case RuleEntry(_, rule) => {
                val testedRule = testRule(rule, stream)
                testedRule.map(ref => List(ref))
            }
            case element: SequenceElement => {
                parseSequenceElement(element)
            }
            case element: OptionsElement => {
                parseOptionElement(element)
            }
            case element: MultiElement => {
                parseMultiElement(element)
            }
            case element: NecessaryElement => {
                parseNecessaryElement(element)
            }
        }
    }

}
type SyntaxResult = Option[List[Node]];

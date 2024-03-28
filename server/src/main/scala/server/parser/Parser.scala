package server.parser

import server.lexer.{Region, TokenRecord}
import server.parser.SyntaxResult.Match

import scala.annotation.tailrec

final class Parser(private val entryRule: Rule) {

    def parse(records: List[TokenRecord]): RuleResult = {
        val stream = ModifiableTokenStream(records)
        val result = testRule(entryRule, stream)
        if (stream.hasLeft) {
            val lastMatch = stream.current()
            RuleResult.RuleFailure(lastMatch.region, entryRule, entryRule.expression)
        } else {
            result
        }
    }

    private def testRule(rule: Rule, stream: ModifiableTokenStream): RuleResult = {
        if (!stream.hasLeft) {
            return RuleResult.RuleNoMatch()
        }
        val startRegion = stream.current().region
        val result = testExpression(rule, rule.expression, stream)
        val currentOrLast = stream.currentOrLast()
        val endPosition = currentOrLast.region.to
        val region = startRegion.setRight(endPosition)
        result match {
            case SyntaxResult.Match(elements) => {
                RuleResult.RuleMatch(Node(rule.name, region, elements, currentOrLast, false))
            }
            case SyntaxResult.NoMatch()                      => RuleResult.RuleNoMatch()
            case SyntaxResult.Failure(region, rule, element) => RuleResult.RuleFailure(region, rule, element)
        }
    }

    private def testExpression(
        owningRule: Rule,
        expression: GrammarElement,
        stream: ModifiableTokenStream
    ): SyntaxResult = {
        // <editor-fold desc="Parsing">

        def parseLiteral(tokenElement: Literal): SyntaxResult = {
            if (!stream.hasLeft) {
                SyntaxResult.NoMatch()
            } else {
                val record = stream.current()
                if (record.token == tokenElement.token) {
                    stream.consume()
                    SyntaxResult.Match(List(Node(tokenElement.token.alias, record.region, List(), record, true)))
                } else {
                    SyntaxResult.NoMatch()
                }
            }
        }

        def parseSequenceElement(sequenceElement: SequenceElement): SyntaxResult = {
            def testElements(elements: List[GrammarElement]): SyntaxResult = {
                elements match {
                    case Nil => SyntaxResult.Match(List())
                    case head :: tail => {
                        val subExpr = testExpression(owningRule, head, stream)
                        subExpr.and(() => testElements(tail))
                    }
                }
            }

            testElements(sequenceElement.expressions)
        }
        def parseOptionElement(optionsElement: OptionsElement): SyntaxResult = {
            @tailrec
            def testElements(elements: List[GrammarElement]): SyntaxResult = {
                elements match {
                    case Nil => SyntaxResult.NoMatch()
                    case head :: tail => {
                        val tokenPosition = stream.index()
                        val subExpr = testExpression(owningRule, head, stream)
                        subExpr match {
                            case SyntaxResult.Match(elements) => SyntaxResult.Match(elements)
                            case SyntaxResult.NoMatch() => {
                                stream.reset(tokenPosition)
                                testElements(tail)
                            }
                            case fail: SyntaxResult.Failure => fail
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
                case SyntaxResult.Match(elements) => syntaxResult
                case SyntaxResult.NoMatch() =>
                    SyntaxResult.Failure(currentOrLastRecord.region, owningRule, necessaryElement.element)
                case fail: SyntaxResult.Failure => fail
            }
        }
        def parseMultiElement(multiElement: MultiElement): SyntaxResult = {
            multiElement.count match {
                case Multiplicity.ZeroOrOne => {
                    val pos = stream.index()
                    stream.reset(pos)
                    val subResult = testExpression(owningRule, multiElement.element, stream)
                    subResult match {
                        case SyntaxResult.Match(elements) => SyntaxResult.Match(elements)
                        case SyntaxResult.NoMatch() => {
                            stream.reset(pos)
                            SyntaxResult.Match(List())
                        }
                        case fail: SyntaxResult.Failure => fail
                    }
                }
                case Multiplicity.ZeroOrMore => {
                    var nodes = List[Node]()
                    var latest = SyntaxResult.Match(List());
                    // that's a do while loop
                    while ({
                        val pos = stream.index()
                        latest = testExpression(owningRule, multiElement.element, stream)
                        latest match {
                            case SyntaxResult.Match(elements) => {
                                nodes = nodes ++ elements
                                true
                            }
                            case SyntaxResult.NoMatch() => {
                                stream.reset(pos)
                                false
                            }
                            case fail: SyntaxResult.Failure => false
                        }
                    }) ()
                    latest match {
                        case SyntaxResult.Match(elements) => SyntaxResult.Match(nodes)
                        case SyntaxResult.NoMatch()       => SyntaxResult.Match(nodes)
                        case fail: SyntaxResult.Failure   => fail
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
                testedRule.asSyntaxResult
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

enum SyntaxResult {
    case Match(elements: List[Node])
    case NoMatch()

    case Failure(region: Region, rule: Rule, failedElement: GrammarElement)

    def and(other: () => SyntaxResult): SyntaxResult = {
        this match {
            case SyntaxResult.Match(elements) => {
                other.apply() match {
                    case SyntaxResult.Match(otherElements) => {
                        SyntaxResult.Match(elements ++ otherElements)
                    }
                    case rest => rest
                }
            }
            case rest => rest
        }
    }
}

enum RuleResult {
    case RuleMatch(node: Node)
    case RuleNoMatch()
    case RuleFailure(region: Region, rule: Rule, failedElement: GrammarElement)

    def asSyntaxResult: SyntaxResult = {
        this match {
            case RuleMatch(node)                    => SyntaxResult.Match(List(node))
            case RuleNoMatch()                      => SyntaxResult.NoMatch()
            case RuleFailure(region, rule, element) => SyntaxResult.Failure(region, rule, element)
        }
    }

}

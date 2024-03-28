package server.lexer

import server.LanguageException
import server.files.InternalFile

import scala.annotation.tailrec

case class Tokenizer(tokens: List[Token]) {

    def tokenize(file: InternalFile, lines: List[String]): TokenizerResult = {
        var result = TokenizerResult.Match(List())
        lines.zipWithIndex.foreach { case (line, index) =>
            result = result.and(() => tokenizeLine(file, line, index));
        }
        result
    }

    def tokenizeLine(file: InternalFile, line: String, lineIndex: Int): TokenizerResult = {

        @tailrec
        def findFirstMatch(line: String, index: Int, tokens: List[Token]): Option[(Token, Int)] = {
            tokens match {
                case Nil => None
                case token :: rest => {
                    token.matches(line, index) match {
                        case TokenMatch.Matched(next) => Some(token, next)
                        case TokenMatch.Unmatched()   => findFirstMatch(line, index, rest)
                    }
                }
            }
        }

        def nextRecord(
            foundToken: Option[(Token, Int)],
            startPosition: Position,
            position: Int
        ): (Option[TokenRecord], Int) = {
            foundToken match {
                case Some((token, nextIndex)) => {
                    val endPosition = Position(lineIndex, nextIndex)
                    val region = Region(file, startPosition, endPosition)
                    val literal = line.substring(position, Math.min(nextIndex, line.length))
                    (Some(TokenRecord(token, literal, region)), nextIndex);
                }
                case None => {
                    val literal = line.substring(position, Math.min(position + 1, line.length))
                    val endPosition = Position(lineIndex, position + 1)
                    val region = Region(file, startPosition, endPosition)
                    (None, position + 1)
                }
            }
        }

        var records = List[TokenRecord]()
        var char = 0
        while (char < line.length) {
            val startPosition = Position(lineIndex, char);
            val test = findFirstMatch(line, char, tokens)
            val token = nextRecord(test, startPosition, char)
            if (token._2 <= char) {
                return TokenizerResult.InvalidIndex(
                  Region(file, startPosition, startPosition.expandRight(1)),
                  tokens.head
                )
            }
            char = token._2
            token._1 match {
                case Some(record) => records = records :+ record
                case None => {
                    throw new LanguageException(Region(file, startPosition, startPosition.expandRight(1)), "Unknown Token")
                }
            }
        }
        TokenizerResult.Match(records)
    }

}

enum TokenizerResult {
    case Match(tokens: List[TokenRecord])
    case InvalidIndex(region: Region, token: Token)
    case UnknownToken(region: Region)

    def and(function: () => TokenizerResult): TokenizerResult = {
        this match {
            case TokenizerResult.Match(tokens) => {
                function() match {
                    case TokenizerResult.Match(newTokens)            => TokenizerResult.Match(tokens ++ newTokens)
                    case TokenizerResult.InvalidIndex(region, token) => TokenizerResult.InvalidIndex(region, token)
                    case TokenizerResult.UnknownToken(region)        => TokenizerResult.UnknownToken(region)
                }
            }
            case TokenizerResult.InvalidIndex(region, token) => TokenizerResult.InvalidIndex(region, token)
            case TokenizerResult.UnknownToken(region)        => TokenizerResult.UnknownToken(region)
        }
    }

    def getOrThrow(): List[TokenRecord] = {
        this match {
            case TokenizerResult.Match(tokens) => tokens
            case TokenizerResult.InvalidIndex(region, token) => throw new LanguageException(region, s"Invalid index for token: $token")
            case TokenizerResult.UnknownToken(region) => throw new LanguageException(region, s"Unknown Token")
        }
    }
}

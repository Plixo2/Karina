package karina.lexer

import karina.LanguageException
import karina.files.InternalFile

import scala.annotation.tailrec

case class Tokenizer(tokens: List[Token]) {

    def tokenize(file: InternalFile, lines: List[String]): List[TokenRecord] = {
        var result = List[TokenRecord]()
        lines.zipWithIndex.foreach { case (line, index) =>
            result = result ++ tokenizeLine(file, line, index);
        }
        result
    }

    def tokenizeLine(file: InternalFile, line: String, lineIndex: Int): List[TokenRecord] = {

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
                val region = Region(file, startPosition, startPosition.expandRight(1))
                throw new LanguageException(region, s"Invalid index for token: $tokens.head")
            }
            char = token._2
            token._1 match {
                case Some(record) => records = records :+ record
                case None => {
                    throw new LanguageException(
                      Region(file, startPosition, startPosition.expandRight(1)),
                      "Unknown Token"
                    )
                }
            }
        }
        records.filter(record => record.token != WhiteSpaceToken && record.token != CommentToken)
    }

}

package server.lexer

import server.lexer.*

object LanguageToken {

    // this list has to be ordered
    def allTokens(): List[Token] = {
        List(
          WhiteSpaceToken,
          CommentToken,
          StringToken,
          LiteralToken("->"),
          LiteralToken("fn"),
          LiteralToken("let"),
          LiteralToken("if"),
          LiteralToken("else"),
          CharToken('+'),
          CharToken(':'),
          CharToken('='),
          CharToken('('),
          CharToken(')'),
          CharToken('{'),
          CharToken('}'),
          CharToken(','),
          CharToken('.'),
          WordToken,
          RegexToken("number", "[0-9]+\\.?[0-9]*".r)
        )
    }
}

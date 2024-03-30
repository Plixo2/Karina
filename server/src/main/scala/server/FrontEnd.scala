package server

import server.files.InternalFile
import server.highlevel.HLParser
import server.interpreter.Environment
import server.lexer.*
import server.parser.*
import server.typed.Program
import server.typed.checker.Checker
import server.typed.parsing.TypedParser


class FrontEnd(val grammarFile: InternalFile) {
    private val grammarEntry: String = "program"

    def parse(file: InternalFile): Unit = {
        val languageTokens = LanguageToken.allTokens()
        val grammar = loadGrammar(languageTokens)
        val tokenizer = Tokenizer(languageTokens)
        val entryRule = grammar.findRule(grammarEntry).expect(s"Rule '$grammarEntry'")
        val parser = Parser(entryRule)
        val records = tokenize(file, tokenizer)
        val result = parser.parse(records)
        result match {
            case RuleResult.RuleMatch(entry) => {
                entry.assertType("program")
                println("High Level:")
                val statements = HLParser.parseProgram(entry)
                statements.foreach(println(_))
                println("Typed:")
                val typed = TypedParser.parseProgram(statements)
                val program = Program(entry.region, typed)
                program.statements.foreach(println(_))
                Checker.checkProgram(program)

                val environment = Environment(program)
                val resultOfProgram = environment.evalValue()
                println("-" * 20)
                println(s"Result: $resultOfProgram")
                println("-" * 20)
            }
            case RuleResult.RuleNoMatch() => {
                throw new LanguageException(file.startRegion(), s"No match for Rule '$grammarEntry'")
            }
            case RuleResult.RuleFailure(region, rule, element) =>
                throw new LanguageException(
                  region,
                  s"Failure of Element ${element} in Rule '$rule' \n${element.region.prettyString()}\nin Source"
                )
        }
    }

    private def tokenize(file: InternalFile, tokenizer: Tokenizer): List[TokenRecord] = {
        val records = tokenizer.tokenize(file, file.lines()).getOrThrow()
        records.filter(record => record.token != WhiteSpaceToken && record.token != CommentToken)
    }

    private def loadGrammar(tokens: List[Token]): RuleSet = {
        val grammar = Grammar(grammarFile)
        grammar.parse(tokens)
    }

}

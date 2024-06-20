package karina

import karina.codegen.bin.{BinCodeGen, BinPrinterPlain}
import karina.codegen.rust.{RustCodeGen, RustPrinter}
import karina.files.{InternalFile, ObjectPath, readFile}
import karina.highlevel.*
import karina.lexer.*
import karina.parser.*
import karina.typed.*
import karina.types.*

import scala.collection.parallel.CollectionConverters.*

object FrontEnd {

    def parse(file: InternalFile, grammarFile: InternalFile): Unit = {
        val languageTokens = LanguageToken.allTokens()
        val grammar = Grammar(grammarFile).parse(languageTokens)
        val tokenizer = Tokenizer(languageTokens)
        val entryRule = grammar.findRule("unit").expect(s"Rule 'unit'")
        val parser = Parser(entryRule)
        val records = tokenizer.tokenize(file, file.lines());
        val result = parser.parse(records)
        if (result.isEmpty) {
            throw new LanguageException(
              file.startRegion(),
              s"No match for Rule '$entryRule'"
            )
        }
        val unitNode = result.get
        val hlUnit = HLParser.parse(unitNode)
        val rootPackage = HLPackage("root", List(("test", hlUnit)), List())
        val default = TypedTransformer.toTypedRoot(rootPackage)
        val types = TypeResolveTransformer.resolve(default)
        val variables = VariableResolveTransformer.resolve(types)
        val typed = InferTransformer.resolve(variables)
//        val code = BinCodeGen.generate(typed)
//        BinPrinterPlain.write("test.main", code, "resources/test.krnac")
        val rustGen = RustCodeGen.generate(typed, "start")
        RustPrinter.write(rustGen, "resources/src/main.rs")
    }

    

}

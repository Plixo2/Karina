package karina

import karina.codegen.bin.{BinCodeGen, BinPrinterPlain}
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
        val default = toTypedRoot(rootPackage)
        val types = TypeResolver.resolve(default)
        val variables = VariableResolver.resolve(types)
        val typed = TypeChecker.resolve(variables)
        val code = BinCodeGen.generate(typed)
        BinPrinterPlain.write(code, "resources/test.krnac")
    }

    private def toTypedRoot(hlPackage: HLPackage): DefaultPackage = {
        val subPath = ObjectPath(List(), allowEmpty = true)
        DefaultPackage(
          hlPackage.name,
          subPath,
          hlPackage.units.par.map(ref => toTypedUnit(subPath, ref._1, ref._2)).toList,
          hlPackage.subPackages.par.map(ref => toTypedPackage(subPath, ref)).toList
        )
    }

    private def toTypedPackage(path: ObjectPath, hlPackage: HLPackage): DefaultPackage = {
        val subPath = path.add(hlPackage.name)
        DefaultPackage(
          hlPackage.name,
          subPath,
          hlPackage.units.par.map(ref => toTypedUnit(subPath, ref._1, ref._2)).toList,
          hlPackage.subPackages.par.map(ref => toTypedPackage(subPath, ref)).toList
        )
    }

    private def toTypedUnit(path: ObjectPath, name: String, hlUnit: HLUnit): DefaultUnit = {
        val subPath = path.add(name)
        DefaultUnit(
          hlUnit.region,
          name,
          subPath,
          hlUnit.imports.par.map(ref => Import(ref.region, ref.path)).toList,
          hlUnit.classes.par.map(ref => toTypedClass(subPath, ref)).toList,
          hlUnit.functions.par.map(ref => toTypedFunction(subPath, true, ref)).toList
        )
    }
    private def toTypedClass(path: ObjectPath, hlClass: HLClass): DefaultClass = {
        val subPath = path.add(hlClass.name)
        val definedGenerics = hlClass.genericHint
            .map(ref => ref.names.map(name => GenericType(ref.region, name, GenericSource.Class(subPath))))
            .getOrElse(List())
        val parameters = hlClass.parameters.map(ref => Parameter(ref.region, ref.name, ref.tpe, None))
        val fields = hlClass.fields.map(ref => { Field(ref.region, ref.name, ref.tpe, ref.value) })
        val functions = hlClass.functions.map(ref => toTypedFunction(subPath, false, ref))
        val superClass = hlClass.superClass.map(ref => {
            Inheritance(ref.region, ref.tpe, ref.memberInits)
        })
        DefaultClass(hlClass.region, hlClass.name, subPath, definedGenerics, parameters, fields, functions, superClass)
    }

    private def toTypedFunction(path: ObjectPath, isStatic: Boolean, hlFunction: HLFunction): DefaultFunction = {
        val subPath = path.add(hlFunction.name)
        val returnType = hlFunction.returnType
        val parameters = hlFunction.parameters.map(ref => Parameter(ref.region, ref.name, ref.tpe, None))
        val definedGenerics = hlFunction.genericHint
            .map(ref => ref.names.map(name => GenericType(ref.region, name, GenericSource.Function(subPath))))
            .getOrElse(List())
        DefaultFunction(
          hlFunction.region,
          isStatic,
          hlFunction.name,
            subPath,
          hlFunction.modifier,
          returnType,
          parameters,
          definedGenerics,
          hlFunction.expression
        )
    }

}

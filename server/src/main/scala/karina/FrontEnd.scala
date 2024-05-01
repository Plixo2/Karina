package karina

import karina.files.{InternalFile, ObjectPath, readFile}
import karina.highlevel.*
import karina.lexer.*
import karina.parser.*
import karina.typed.*
import karina.types.*;

class FrontEnd() {

    def parse(file: InternalFile): Unit = {
        val languageTokens = LanguageToken.allTokens()
        val grammar = Grammar(readFile("resources/grammar.txt")).parse(languageTokens)
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
        val root = CompilePackage("root", List(NodeUnit("test", hlUnit)), List())
        test(root, None, root)
    }

    private def test(
        root: CompilePackage,
        location: Option[ObjectPath],
        compilePackage: CompilePackage
    ): CompilePackage = {
        CompilePackage(
          compilePackage.name(),
          compilePackage.units.map(ref =>
              testUnit(root, location.map(_.add(ref.name())).getOrElse(ObjectPath(List(ref.name()))), ref)
          ),
          compilePackage.subPackages.map(ref =>
              test(root, Some(location.map(_.add(ref.name())).getOrElse(ObjectPath(List(ref.name())))), ref)
          )
        )
    }
    private def testUnit(root: CompilePackage, location: ObjectPath, compileUnit: CompileUnit): CompileUnit = {
        val nodeUnit = compileUnit.asInstanceOf[NodeUnit].hlUnit
        val imports = nodeUnit.imports
        for (elem <- imports) {
            root.locateClass(elem.path) match {
                case Some(value) => {
                    println(s"Found import '${elem}'")
                }
                case None =>
                    throw new LanguageException(
                      elem.region,
                      s"Import '${elem}' not found"
                    )
            }
        }
        for (elem <- nodeUnit.classes) {
            transformClass(root, location.add(elem.name), elem)
        }

        compileUnit
    }

    private def transformClass(root: CompilePackage, location: ObjectPath, hlClass: HLClass): typed.Class = {
        val genericNames = hlClass.genericHint.map(_.names).getOrElse(List())
        val genericTypes = genericNames.map(GenericType(_, GenericSource.Class(location)))
        val lookup = TypeLookup(root, genericTypes, List())
        val parameters = hlClass.parameter.map(param => (param.name, transformType(lookup, param.tpe)))
        val fields = hlClass.fields.map(field => Field(field.name, transformType(lookup, field.tpe), field.value))
        val functions = hlClass.functions.map(func => NodeFunction(func))
        ClassWithOutInheritance(hlClass.name, genericTypes, parameters, fields, functions, hlClass.superClass)
    }

    private def transformType(lookup: TypeLookup, hlType: HLType): Type = {
        hlType match {
            case HLClassType(region, path, genericHint) => {
                lookup.lookup(HLClassType(region, path, genericHint))
            }
            case HLArrayType(region, inner) => ArrayType(transformType(lookup, inner))
            case HLFunctionType(region, parameter, returnType) => {
                throw new LanguageException(
                  region,
                  s"Function types are not supported"
                )
            }
            case HLInt()   => IntegerType()
            case HLFloat() => FloatType()
            case HLBool()  => BooleanType()
            case HLVoid()  => VoidType()
        }
    }

    private case class TypeLookup(
        root: CompilePackage,
        generics: List[GenericType],
        imports: List[HLImport]
    ) {
        def lookup(classType: HLClassType): Type = {
            classType match {
                case HLClassType(region, path, genericHint) => {
                    if (path.length == 1) {
                        generics.find(_.name == path.head) match {
                            case Some(value) => return value
                            case None        => {}
                        }
                    }
                    root.locateClass(path) match {
                        case Some(value) => {
                            val genOfClass = value.definedGeneric()
                            if (genericHint.map(_.types.length).getOrElse(0) != genOfClass.length) {
                                throw new LanguageException(
                                  region,
                                  s"Class '${path}' expects ${genOfClass.length} generic parameters"
                                )
                            }
                            val transformedTypes =
                                genericHint.map(_.types.map(transformType(this, _))).getOrElse(List())
                            val mapping = genOfClass.zip(transformedTypes)
                            ObjectType(path, mapping.toMap)
                        }
                        case None =>
                            throw new LanguageException(
                              region,
                              s"Class '${path}' not found"
                            )
                    }
                }
            }
        }
    }
}

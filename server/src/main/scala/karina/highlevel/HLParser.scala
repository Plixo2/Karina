package karina.highlevel

import karina.files.ObjectPath
import karina.highlevel.HLParser.parseGenericHint
import karina.parser.Node
import karina.types.{ArrayType, BooleanType, FloatType, FunctionType, IntegerType, BaseType, ObjectTypeDefault, StringType, Type, VoidType}
import karina.{LanguageException, parser}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.HashMap

object HLParser {

    def parse(node: Node): HLUnit = {
        node.assertType("unit")
        var imports = List[HLImport]()
        var classes = List[HLStruct]()
        var functions = List[HLFunction]()
        val items = node.getAll("item")
        items.foreach(item => {
            if (item.has("import")) {
                val importNode = item("import")
                val objectPath = parseWordChain(importNode("dotWordChain"))
                imports = imports :+ HLImport(importNode.region, objectPath)
            } else if (item.has("class")) {
                classes = classes :+ parseClass(item("class"))
            } else if (item.has("function")) {
                functions = functions :+ parseFunction(item("function"))
            } else {
                throw new LanguageException(item.region, "Unknown item type")
            }
        });
        HLUnit(node.region, imports, classes, functions)
    }

    private def parseClass(node: Node): HLStruct = {
        node.assertType("class")
        val name = node.id()
        val genHint = node.get("genericHintDefinition").map(parseGenericHintDefinition)
        val fields = node.map(
          "field",
          field => {
              val name = field.id()
              val type_ = HLTypeParser.parse(field("type"))
              HLField(field.region, name, type_)
          }
        )
        val functions = node.map("function", parseFunction)
        HLStruct(node.region, name, genHint, fields, functions)
    }

    private def parseFunction(node: Node): HLFunction = {
        node.assertType("function")
        val mods = node.get("functionModifier") match {
            case Some(node) => {
                if (node.has("override")) {
                    Modifier.Override
                } else if (node.has("native")) {
                    Modifier.Native
                } else if (node.has("virtual")) {
                    Modifier.Virtual
                } else {
                    throw new LanguageException(node.region, "Unknown function modifier")
                }
            }
            case None => {
                Modifier.Normal
            }
        }
        val name = node.id()
        val expression = if (node.has("block")) {
            HLExpressionParser.parseBlock(node("block"))
        } else {
            HLExpressionParser.parse(node("expression"))
        }
        val genHint = node.get("genericHintDefinition").map(parseGenericHintDefinition)
        val parameters = parseParameterList(node("parameterList"))
        val returnType = node.get("type").map(HLTypeParser.parse).getOrElse(VoidType(node("id").region))

        HLFunction(node.region, name, mods, genHint, parameters, returnType, expression)
    }

    def parseInitList(node: Node): List[HLInitMember] = {
        node.assertType("initList")
        node.map(
          "memberInit",
          member => {
              val name = member.id()
              val expression = HLExpressionParser.parse(member("expression"))
              HLInitMember(member.region, name, expression)
          }
        )
    }

    private def parseParameterList(node: Node): List[HLParameter] = {
        node.assertType("parameterList")
        node.map(
          "parameter",
          param => {
              val name = param.id()
              val type_ = HLTypeParser.parse(param("type"))
              HLParameter(param.region, name, type_)
          }
        )
    }

    def parseWordChain(node: Node): ObjectPath = {
        ObjectPath(node.getAll("id").map(_.id()), allowEmpty = false)
    }

    def parseGenericHint(node: Node): List[Type] = {
        node.assertType("genericHint")
        val types = node.map("type", f => HLTypeParser.parse(f))
        types
    }

    private def parseGenericHintDefinition(node: Node): HLGenericDefinition = {
        node.assertType("genericHintDefinition")
        val names = node.map("id", _.id())
        HLGenericDefinition(node.region, names)
    }
}
object HLTypeParser {

    def parse(node: Node): Type = {
        node.assertType("type")
        if (node.has("void")) {
            VoidType(node.region)
        } else if (node.has("int")) {
            IntegerType(node.region)
        } else if (node.has("float")) {
            FloatType(node.region)
        } else if (node.has("string")) {
            StringType(node.region)
        }else if (node.has("bool")) {
            BooleanType(node.region)
        } else if (node.has("dotWordChain")) {
            val path = HLParser.parseWordChain(node("dotWordChain"))
            val genericHint = node.get("genericHint").map(parseGenericHint).getOrElse(List())
            ObjectTypeDefault(node.region, path, genericHint)
        } else if (node.has("arrayType")) {
            val arrayType = node("arrayType")("type")
            ArrayType(node.region, parse(arrayType))
        } else if (node.has("functionType")) {
            val functionType = node("functionType")
            val returnType = functionType.get("type").map(parse).getOrElse(VoidType(functionType.region))
            val parameters = functionType("typeList").map("type", parse)
            FunctionType(node.region, parameters, returnType)
        } else if (node.has("?")) {
            BaseType(node.region)
        } else {
            throw new LanguageException(node.region, "Unknown type")
        }
    }

}

object HLExpressionParser {

    def parse(node: Node): Expression = {
        node.assertType("expression")
        if (node.has("variableDefinition")) {
            val varDef = node("variableDefinition")
            val name = varDef.id()
            val type_ = varDef.get("type").map(HLTypeParser.parse)
            val expression = parse(varDef("expression"))
            HLVarDef(node.region, name, type_, expression)
        } else if (node.has("return")) {
            if (node.has("expression")) {
                val expression = parse(node("expression"))
                Return(node.region, Some(expression))
            } else {
                Return(node.region, None)
            }
        } else if (node.has("closure")) {
            val closure = node("closure")
            val returnType = closure.get("type").map(HLTypeParser.parse)
            val expression = parse(closure("expression"))
            val parameters = closure("closureTypeList").map(
              "closureType",
              param => {
                  val name = param.id()
                  val type_ = param.get("type").map(HLTypeParser.parse)
                  HLClosureParameter(param.region, name, type_)
              }
            )
            HLClosure(node.region, parameters, returnType, expression)
        } else if (node.has("conditionalOrExpression")) {
            parseBinExpr(
              node("conditionalOrExpression"),
              List(
                "conditionalOrExpression",
                "conditionalAndExpression",
                "equalityExpression",
                "relationalExpression",
                "additiveExpression",
                "multiplicativeExpression",
                "unaryExpression"
              )
            )
        } else {
            throw new LanguageException(node.region, "Unknown expression")
        }
    }

    private def parseBinExpr(node: Node, binExprLeft: List[String]): Expression = {
        val current = binExprLeft.head
        node.assertType(current)
        if (binExprLeft.length == 1) {
            parseUnary(node)
        } else {
            val next = binExprLeft(1)
            val left = parseBinExpr(node(next), binExprLeft.tail)
            if (node.has(current)) {
                val right = parseBinExpr(node(current), binExprLeft)
                HLBinary(node.region, left, right, getBIOperator(node))
            } else {
                left
            }
        }
    }

    private def parseUnary(node: Node): Expression = {
        node.assertType("unaryExpression")
        val factor = parseFactor(node("factor"))
        if (node.has("-")) {
            Unary(node.region, factor, UnaryOP.Negate)
        } else if (node.has("!")) {
            Unary(node.region, factor, UnaryOP.Not)
        } else {
            factor
        }
    }
    
   

    private def parseFactor(node: Node): Expression = {
        node.assertType("factor")
        val left = parsePostFix(parseObject(node("object")), node.getAll("postFix"))
        if (node.has("expression")) { 
            Assign(node.region, left, parse(node("expression")))
        } else {
            left
        }
    }

    @tailrec
    private def parsePostFix(prev: Expression, nodes: List[Node]): Expression = {
        if (nodes.isEmpty) {
            return prev
        }
        val node = nodes.head
        node.assertType("postFix")
        val expression = if (node.has("id")) {
            HLDotName(node.region, prev, node.id())
        } else if (node.has("expressionList")) {
            val expressions = node("expressionList")
            HLCall(node.region, prev, parseExpressionList(expressions))
        } else if (node.has("expression")) {
            val index = parse(node("expression"))
            HLArrayIndex(node.region, prev, index)
        } else {
            throw new LanguageException(node.region, "Unknown postFix")
        }
        parsePostFix(expression, nodes.tail)
    }


    private def parseObject(node: Node): Expression = {
        node.assertType("object")
        if (node.has("if")) {
            val ifNode = node("if")
            val condition = parse(ifNode("expression"))
            val thenExpression = parseBlock(ifNode("block"))
            val elseExpression = ifNode.get("branchOpt").map(ref => parse(ref("expression")))

            val caseCheck = ifNode
                .get("type")
                .map(ref => {
                    val caseCheck = if (ifNode.has("id")) {
                        HLCaseCheck.Name(ifNode.id())
                    } else if (ifNode.has("commaWordChain")) {
                        HLCaseCheck.Destructor(parseCommaWordChain(ifNode("commaWordChain")))
                    } else {
                        HLCaseCheck.None()
                    }
                    val type_ = HLTypeParser.parse(ref)
                    HLBranchCaseCheck(ref.region, type_, caseCheck)
                })

            HLBranch(
              node.region,
              condition,
              caseCheck,
              thenExpression,
              elseExpression
            )
        } else if (node.has("while")) {
            val whileNode = node("while")
            val expression = parse(whileNode("expression"))
            val block = parseBlock(whileNode("block"))
            While(node.region, expression, block)
        } else if (node.has("for")) {
            val forNode = node("for")
            val varName = forNode.id()
            val iter = parse(forNode("expression"))
            val block = parseBlock(forNode("block"))
            HLFor(node.region, varName, iter, block)
        } else if (node.has("block")) {
            parseBlock(node("block"))
        } else if (node.has("array")) {
            val array = node("array")
            Array(array.region, parseExpressionList(array("expressionList")))
        } else if (node.has("expression")) {
            parse(node("expression"))
        } else if (node.has("number")) {
            val numStr = node.number()
            if (numStr.contains(".")) {
                numStr.toDoubleOption match {
                    case Some(value) => {
                        FloatLiteral(node.region, value)
                    }
                    case None => {
                        throw new LanguageException(node.region, "Invalid float number")
                    }
                }
            } else {
                numStr.toLongOption match {
                    case Some(value) => {
                        IntLiteral(node.region, value)
                    }
                    case None => {
                        throw new LanguageException(node.region, "Invalid int number")
                    }
                }
            }
        } else if (node.has("break")) {
            Break(node.region)
        } else if (node.has("id")) {
            HLName(node.region, node.id())
        } else if (node.has("stringLiteral")) {
            val content = node.string()
            StringLiteral(node.region, content.substring(1, content.length - 1))
        } else if (node.has("self")) {
            HLSelf(node.region)
        } else if (node.has("super")) {
            HLSuper(node.region)
        } else if (node.has("true")) {
            BoolLiteral(node.region, true)
        } else if (node.has("false")) {
            BoolLiteral(node.region, false)
        } else if (node.has("new")) {
            val returnNode = node("new")
            val tpe = HLTypeParser.parse(returnNode("type"))
            val initList = returnNode.get("initList").map(HLParser.parseInitList).getOrElse(List())
            HLNew(node.region, tpe, initList)
        } else {
            throw new LanguageException(node.region, "Unknown object")
        }
    }
    
    private def parseExpressionList(node: Node): List[Expression] = {
        node.assertType("expressionList")
        node.map("expression", parse)
    }

    def parseBlock(node: Node): Expression = {
        node.assertType("block")
        val expressions = node.map("expression", parse)
        HLBlock(node.region, expressions)
    }

    private def parseCommaWordChain(node: Node): List[String] = {
        node.assertType("commaWordChain")
        node.getAll("id").map(_.id())
    }

    private def getBIOperator(node: Node): BinaryOP = {
        val mappings = mutable.HashMap[String, BinaryOP]()
        mappings += ("+" -> BinaryOP.Add)
        mappings += ("&" -> BinaryOP.Concat)
        mappings += ("-" -> BinaryOP.Subtract)
        mappings += ("*" -> BinaryOP.Multiply)
        mappings += ("/" -> BinaryOP.Divide)
        mappings += ("%" -> BinaryOP.Modulo)
        mappings += ("==" -> BinaryOP.Equal)
        mappings += ("!=" -> BinaryOP.NotEqual)
        mappings += (">" -> BinaryOP.GreaterThan)
        mappings += ("<" -> BinaryOP.LessThan)
        mappings += (">=" -> BinaryOP.GreaterThanOrEqual)
        mappings += ("<=" -> BinaryOP.LessThanOrEqual)
        mappings += ("&&" -> BinaryOP.And)
        mappings += ("||" -> BinaryOP.Or)

        val (_, op) = mappings
            .find((key, value) => {
                node.has(key)
            })
            .getOrElse(throw new LanguageException(node.region, "Unknown binary operator"))

        op
    }

}

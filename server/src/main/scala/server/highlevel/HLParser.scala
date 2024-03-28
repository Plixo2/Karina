package server.highlevel

import server.LanguageException
import server.parser.Node

object HLParser {

    def parse_program(node: Node): List[HLStatement] = {
        node.assertType("program");
        val statements = node.map("let", parse_let)
        val last = node("id").expect("variable terminal")
        statements :+ HLVariableTerminator(last.id(), last.region)
    }

    private def parse_let(node: Node): HLStatement = {
        node.assertType("let");

        val name = node("id").expect("identifier on let statement").id()
        val expressionNode = node("expression").expect("expression on let statement")
        val expression = parse_expression(expressionNode)
        HLLet(name, expression, node.region)
    }

    private def parse_expression(node: Node): HLExpression = {
        node.assertType("expression");
        if (node.has("number")) {
            val numberNode = node("number").get();
            val numberString = numberNode.number();
            numberString.toIntOption match {
                case Some(number) => HLIntLiteral(number, numberNode.region)
                case None => throw new LanguageException(numberNode.region, "Number cannot be parsed as an integer")
            }
        } else if (node.has("+")) {
            val variables = node.getAll("id")
            if (variables.length != 2) {
                throw new LanguageException(node.region, "Addition expression must have exactly two variables")
            } else {
                val a = variables.head.id()
                val b = variables(1).id()
                HLAddVariables(a, b, node.region)
            }
        } else if (node.has("id")) {
            val id = node.id()
            HLVariable(id, node.region)
        } else {
            throw new LanguageException(node.region, "Expression must be a number, addition or variable")
        }

    }
}

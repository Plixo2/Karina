package karina.lexer

import karina.files.InternalFile

case class Region(file: InternalFile, from: Position, to: Position) {

    def setRight(to: Position): Region = Region(file, from, to)

    override def toString: String = {
        s"$from -> $to"
    }

    private def UIRLocation: Option[String] = {
        file.URIString() match {
            case Some(value) => {
                Some(s"in: $value:${(from.line + 1)}:${from.column + 1}")
            }
            case None => None
        }
    }

    def prettyString(): String = {
        var lines = List[String]()
        lines = lines ++ prettyPrintLines()
        UIRLocation match {
            case Some(value) => {
                lines = lines :+ value
            }
            case None => {}
        }
        lines = lines :+ "\n"

        lines.mkString("\n")
    }
    private def prettyPrintLines(): List[String] = {
        val left = from
        val right = to
        val loadedLines = file.lines()
        val minLine = Math.min(loadedLines.length - 1, left.line)
        val maxLine = Math.min(loadedLines.length - 1, right.line)
        if (minLine == maxLine && loadedLines.nonEmpty) {
            val line = loadedLines(minLine)
            val minChar = Math.min(left.column, right.column)
            val maxChar = Math.max(left.column, right.column)
            val dChar = Math.max(maxChar - minChar, 1)
            val paddingLeft = (" " * minChar) + "v" * dChar
            List(paddingLeft, line)
        } else {
            var lines = List[String]()
            lines = lines :+ ((" " * left.column) + "v")
            for (i <- minLine to maxLine) {
                if (i >= minLine && i <= maxLine) {
                    val line = loadedLines(i)
                    lines = lines :+ line
                }
            }
            lines = lines :+ ("^" * right.column)
            lines
        }
    }
}

case class Position(line: Int, column: Int) {

    def expandRight(add: Int): Position = Position(line, column + add)

    override def toString: String = {
        s"($line:$column)"
    }

}

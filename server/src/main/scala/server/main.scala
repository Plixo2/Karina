import server.{FrontEnd, readFile}

@main
def main(): Unit = {
    val grammarFile = readFile("resources/grammar.txt")
    val testFile = readFile("resources/test.txt");
    val frontEnd = FrontEnd(grammarFile)
    frontEnd.parse(testFile)
}

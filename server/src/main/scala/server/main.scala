import server.{FrontEnd, read_file}

@main
def main(): Unit = {
    val grammar_file = read_file("resources/grammar.txt")
    val test_file = read_file("resources/test.txt");
    val frontEnd = FrontEnd(grammar_file)
    frontEnd.parse(test_file)
}

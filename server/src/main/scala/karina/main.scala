package karina

import karina.files.readFile

@main
def main(): Unit = {
    val startTime = System.nanoTime()

    val testFile = readFile("resources/test.krna");
    val grammar = readFile("resources/grammar.txt");

    FrontEnd.parse(testFile, grammar)

    val endTime = System.nanoTime()
    val duration = (endTime - startTime) / 1000000
    println(s"Execution time: $duration ms")

}

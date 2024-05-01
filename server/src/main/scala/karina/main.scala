import karina.FrontEnd
import karina.files.readFile

@main
def main(): Unit = {
    val testFile = readFile("resources/test.krna");
    val frontEnd = FrontEnd()
    frontEnd.parse(testFile)
}

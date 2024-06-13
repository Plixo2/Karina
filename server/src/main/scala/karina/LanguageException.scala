package karina

import karina.lexer.Region
import karina.types.Type

class LanguageException(val region: Region, val msg: String) extends RuntimeException(prettyPrint(region, msg)) {}

private def prettyPrint(region: Region, msg: String) = {
    s"${msg} at ${region.toString}\n${region.prettyString()}"
}

class LanguageTypeException(val region: Region, val typeA: Type, val typeB: Type) extends RuntimeException(prettyPrintType(region, typeA, typeB)) {}

private def prettyPrintType(region: Region, typeA: Type, typeB: Type) = {
    val tA = s"${typeA.getRegion().prettyString()}"
    val tB = s"${typeB.getRegion().prettyString()}"

    s"Type mismatch: \nExpected ${typeA} \n$tA \nGot ${typeB} \n$tB \n\n in Source ${region.toString}\n${region.prettyString()}"
}

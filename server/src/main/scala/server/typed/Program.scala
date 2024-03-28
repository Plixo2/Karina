package server.typed

import server.lexer.Region

class Program(val region: Region, val statements: List[Statement]) {}

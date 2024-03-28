package server

import server.lexer.Region

class LanguageException(val region: Region, val  msg: String) extends RuntimeException(prettyPrint(region, msg)) {
    
}

def prettyPrint(region: Region, msg: String) = {
    s"${msg} at ${region.toString}\n${region.prettyString()}"
}


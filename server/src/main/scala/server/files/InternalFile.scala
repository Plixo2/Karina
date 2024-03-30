package server.files

import server.lexer.{Position, Region}
import server.readFile


sealed trait InternalFile {
    def content: String

    def lines(): List[String]

    final def startRegion(): Region = {
        Region(this, Position(0, 0), Position(0, 1))
    }

    def URIString(): Option[String]
}

case class VirtualFile(content: String, name: String) extends InternalFile {
    def URIString(): Option[String] = Some(name)
    def lines(): List[String] = content.split("\n").toList
}

case class RealFile(content: String, path: String) extends InternalFile {
    def URIString(): Option[String] = {
            Some("file://" + path)
    }
    def lines(): List[String] = content.split("\n").toList
}

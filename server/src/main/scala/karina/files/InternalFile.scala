package karina.files

import karina.lexer.{Position, Region}

import java.io.File
import scala.io.Source

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
        Some("file:///" + path)
    }
    def lines(): List[String] = content.split("\n").toList
}

//read file and get absolut path
def readFile(path: String): RealFile = {
    val file = File(path)
    val source = Source.fromFile(file, "utf8")
    RealFile(source.mkString, file.getAbsolutePath.replace("\\", "/"))
}

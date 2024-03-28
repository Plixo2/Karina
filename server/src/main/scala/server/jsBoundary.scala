package server

import server.files.{InternalFile, RealFile}

import scala.scalajs.js
import scala.scalajs.js.annotation.*


@js.native
@JSImport("fs", JSImport.Namespace)
object Fs extends js.Object {
  def readFileSync(path: String, encoding: String): String = js.native
}


def read_file(path: String): InternalFile = {
  val file = Fs.readFileSync(path, "utf8")
  RealFile(file, path)
}

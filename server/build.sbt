

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.3"

lazy val root = (project in file("."))
  .settings(
    name := "karina"
  )

libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"
libraryDependencies += "org.apache.commons" % "commons-text" % "1.12.0"

libraryDependencies += "org.ow2.asm" % "asm" % "9.5"
//implementation 'org.ow2.asm:asm-commons:9.5'
libraryDependencies += "org.ow2.asm" % "asm-commons" % "9.5"
//implementation 'org.ow2.asm:asm-util:9.5'
libraryDependencies += "org.ow2.asm" % "asm-util" % "9.5"
//implementation 'org.ow2.asm:asm-tree:9.5'
libraryDependencies += "org.ow2.asm" % "asm-tree" % "9.5"

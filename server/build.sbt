

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.3"

lazy val root = (project in file("."))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name := "server",

    scalaJSUseMainModuleInitializer := true,
//    libraryDependencies += "org.scala-js" %%% "scalajs-nodejs" % "1.1.0",
      libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.4.0",
//    libraryDependencies += "net.exoego" %%% "scala-js-nodejs-v16" % "0.14.0",

    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) }

    /* Depend on the scalajs-dom library.
     * It provides static types for the browser DOM APIs.
     */
//    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.4.0"

//    libraryDependencies += "org.scala-js" %% "scalajs-env-jsdom-nodejs" % "1.0.0"
    ,
    //      libraryDependencies += "org.scala-js" %%% "scalajs-nodejs" % "0.14.0",
  )

//libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.4.0"
//libraryDependencies += "org.scala-js" %% "scalajs-env-jsdom-nodejs" % "1.0.0"

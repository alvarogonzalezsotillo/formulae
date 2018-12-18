// VARIOS TARGETS: https://github.com/muuki88/scala-target-examples

// https://github.com/portable-scala/sbt-crossproject


// shadow sbt-scalajs' crossProject and CrossType from Scala.js 0.6.x
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

val sharedSettings = Seq(
  scalaVersion := "2.11.8",
  libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.0" % "test",
  libraryDependencies += "org.scala-lang.modules" %%% "scala-parser-combinators" % "1.0.5"
)

val jsSettings = Seq(
  scalaJSUseMainModuleInitializer := true,
  mainClass := Some("rne.Main"),
  scalaJSModuleKind := ModuleKind.NoModule, // para navegador
  //scalaJSModuleKind :=  ModuleKind.CommonJSModule, // para nodejs
  libraryDependencies += "io.scalajs" %%% "nodejs" % "0.4.2",
  libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.1",
  libraryDependencies += "be.doeraene" %%% "scalajs-jquery" % "0.9.1",
  jsDependencies += "org.webjars" % "jquery" % "2.1.4" / "2.1.4/jquery.js"


  //scalaJSOutputWrapper := ("", "Main.main();")
)

val jvmSettings = Seq(
  mainClass := Some("formulae.ParserMain"),
)

lazy val formulae =
  // select supported platforms
  crossProject(JSPlatform, JVMPlatform/*, NativePlatform*/)
    .crossType(CrossType.Full) // [Pure, Full, Dummy], default: CrossType.Full
    .settings(sharedSettings)
    .jsSettings(jsSettings) // defined in sbt-scalajs-crossproject
    .jvmSettings(jvmSettings)
    //.nativeSettings(/* ... */) // defined in sbt-scala-native

// Optional in sbt 1.x (mandatory in sbt 0.13.x)
lazy val formulaeJS     = formulae.js
lazy val formulaeJVM    = formulae.jvm
//lazy val formulaeNative = formulae.native

addCommandAlias("run","formulaeJVM/run")


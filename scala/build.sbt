scalaVersion := "2.12.4"

autoCompilerPlugins := true

addCompilerPlugin("org.scala-lang.plugins" % "scala-continuations-plugin_2.12.2" % "1.0.3")

libraryDependencies += "org.scala-lang.plugins" %% "scala-continuations-library" % "1.0.3"

scalacOptions += "-P:continuations:enable"

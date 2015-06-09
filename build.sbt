import org.allenai.plugins.CoreDependencies._

organization := "org.allenai.ari"

name := "tableilp-solver"

description := "An example Aristo solver"

enablePlugins(WebServicePlugin)

libraryDependencies ++= Seq(
  allenAiCommon,
  "org.allenai.ari" %% "ari-api" % "0.0.3",
  "org.allenai.ari" %% "ari-solvers-common" % "0.0.3"
)

resolvers += "AllenAI Releases" at "http://utility.allenai.org:8081/nexus/content/repositories/releases"

// You can increase the solver memory settings here, if you need to.
javaOptions ++= Seq(s"-Dlogback.appname=${name.value}")

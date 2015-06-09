import org.allenai.plugins.CoreDependencies._

name := "tableilp-solver"

description := "An ILP based Table Inference solver"

libraryDependencies ++= Seq(
  allenAiCommon
)

// You can increase the solver memory settings here, if you need to.
javaOptions ++= Seq(s"-Dlogback.appname=${name.value}")

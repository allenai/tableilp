import Dependencies._

name := "tableilp-solver"

description := "An ILP based Table Inference solver"

GlobalBuildSettings

SolverJavaSettings

mainClass in Revolver.reStart := Some("org.allenai.ari.solvers.tableilp.TableIlpServer")

libraryDependencies ++= Seq(
  allenAiCommon,
  allenAiGuice,
  "net.sf.opencsv" % "opencsv" % "2.1",
  nlpstack("chunk"),
  nlpstack("tokenize"),
  nlpstack("postag"),
  nlpstack("core")
)

// You can increase the solver memory settings here, if you need to.
javaOptions ++= Seq(s"-Dlogback.appname=${name.value}", "-Djava.library.path=lib")

includeFilter in unmanagedJars := "*.jar" || "*.so" || "*.dylib"

fork := true


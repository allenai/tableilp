import Dependencies._
import NativePackagerHelper.directory

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
  nlpstack("core"),
  "net.debasishg" %% "redisclient" % "3.0"
)

// Copy local data files to the staging directory.
mappings in Universal ++= directory(baseDirectory.value / "data")

mappings in Universal += {
  ((baseDirectory in lucienceSolver).value / "src" / "main" / "resources" / "application.conf") ->
      "conf/lucience.conf"
}

// You can increase the solver memory settings here, if you need to.
javaOptions ++= Seq(s"-Dlogback.appname=${name.value}", "-Djava.library.path=lib")

includeFilter in unmanagedJars := "*.jar" || "*.so" || "*.dylib"

fork := true


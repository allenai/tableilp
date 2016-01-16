package org.allenai.ari.solvers.tableilp

import org.allenai.ari.solvers.HttpSolverServer
import org.allenai.ari.solvers.common.SolversCommonModule
import org.allenai.common.Config._
import org.allenai.common.guice.ActorSystemModule

import com.google.inject.Guice
import com.typesafe.config.ConfigFactory
import net.codingwell.scalaguice.InjectorExtensions.ScalaInjector

/** HTTP server for the solver. This is the entry point to your application. */
object TableIlpServer extends HttpSolverServer {
  // Load the configuration located in src/main/resources/application.conf.
  val rootConfig = ConfigFactory.systemProperties.withFallback(ConfigFactory.load)

  // Load the local config for the solver.
  val localConfig = rootConfig.getConfig("ari.solvers.tableilp")

  // Load SolversCommonModule for EntailmentService
  val injector = Guice.createInjector(
    new ActorSystemModule,
    // TODO(ashish33) Replace localConfig call with ConfigModule instance; needs new AllenAICommon
    new SolversCommonModule(localConfig, true)
  )

  override val port = localConfig[Int]("port")

  // Here, you can read any other config values you need to construct your solver.

  override val solver: TableIlpSolver = {
    logger.info("creating solver")
    val result = injector.instance[TableIlpSolver]
    logger.info("done creating solver!")
    result
  }

  // This line starts the server running.
  start()
}

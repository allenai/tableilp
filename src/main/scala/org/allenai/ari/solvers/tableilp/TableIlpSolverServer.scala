package org.allenai.ari.solvers.tableilp

import org.allenai.ari.solvers.HttpSolverServer
import org.allenai.common.Config._

import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory

/** HTTP server for the solver. This is the entry point to your application. */
object TableIlpSolverServer extends HttpSolverServer {
  // Load the configuration located in src/main/resources/application.conf.
  val rootConfig = ConfigFactory.systemProperties.withFallback(ConfigFactory.load)

  // Load the local config for the solver.
  val localConfig = rootConfig.getConfig("ari.solvers.tableilp")

  override val port = localConfig[Int]("port")

  // Here, you can read any other config values you need to construct your solver.

  override val solver: TableIlpSolver = new TableIlpSolver

  // This line starts the server running.
  start()
}

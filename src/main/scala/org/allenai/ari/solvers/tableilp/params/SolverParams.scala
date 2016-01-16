package org.allenai.ari.solvers.tableilp.params

import com.google.inject.Inject
import com.google.inject.name.Named
import com.typesafe.config.{ ConfigFactory, Config }

/** Various high level parameters of the main Aristo solver.
  *
  * @param useRedisCache use a local Redis cache for entailment scores; requires redis-server
  *   running on localhost:6739
  * @param fullTablesInIlpSolution include entire tables, not just active rows, in IlpSolution
  * @param scienceTermsDatastoreConfig Datastore location of a file containing science terms
  */
class SolverParams @Inject() (
  @Named("solver.useRedisCache") val useRedisCache: Boolean,
  @Named("solver.fullTablesInIlpSolution") val fullTablesInIlpSolution: Boolean,
  @Named("solver.scienceTermsDatastoreFile") val scienceTermsDatastoreConfig: Config
) {}

/** An object to capture the default ILP model parameters */
object SolverParams {
  val Default = new SolverParams(
    useRedisCache = false,
    fullTablesInIlpSolution = false,
    scienceTermsDatastoreConfig = ConfigFactory.empty()
  )
}

package org.allenai.ari.solvers.tableilp.params

import com.google.inject.Inject
import com.google.inject.name.Named

/** Various high level parameters of the main Aristo solver.
  *
  * @param useFallbackSolver if this solver doesn't answer the question, use a fallback solver
  * @param useFallbackSolverCompId whether to use fallback solver's ID or TableIlp solver's ID
  * @param useRedisCache use a local Redis cache for entailment scores; requires redis-server
  *   running on localhost:6739
  * @param fullTablesInIlpSolution include entire tables, not just active rows, in IlpSolution
  */
class SolverParams @Inject() (
  @Named("solver.useFallbackSolver") val useFallbackSolver: Boolean,
  @Named("solver.useFallbackSolverCompId") val useFallbackSolverCompId: Boolean,
  @Named("solver.useRedisCache") val useRedisCache: Boolean,
  @Named("solver.fullTablesInIlpSolution") val fullTablesInIlpSolution: Boolean
) {}

/** An object to capture the default ILP model parameters */
object SolverParams {
  val Default = new SolverParams(
    useFallbackSolver = false,
    useFallbackSolverCompId = true,
    useRedisCache = false,
    fullTablesInIlpSolution = false
  )
}

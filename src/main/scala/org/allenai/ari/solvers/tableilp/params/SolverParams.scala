package org.allenai.ari.solvers.tableilp.params

import com.google.inject.Inject
import com.google.inject.name.Named

/** Various parameters of the ILP model.
  *
  * @param failOnUnansweredQuestions declare question "unanswered" when no answer choice is found
  * @param useFallbackSolver if this solver doesn't answer the question, use a fallback solver
  * @param useFallbackSolverCompId whether to use fallback solver's ID or TableIlp solver's ID
  * @param checkForTies check whether there another answer choice is as good as the best one
  */
class SolverParams @Inject() (
  @Named("solver.failOnUnansweredQuestions") val failOnUnansweredQuestions: Boolean,
  @Named("solver.useFallbackSolver") val useFallbackSolver: Boolean,
  @Named("solver.useFallbackSolverComponentId") val useFallbackSolverCompId: Boolean,
  @Named("solver.checkForTies") val checkForTies: Boolean
) {}

/** An object to capture the default ILP model parameters */
object SolverParams {
  val Default = new SolverParams(
    failOnUnansweredQuestions = true,
    useFallbackSolver = true,
    useFallbackSolverCompId = true,
    checkForTies = true
  )
}

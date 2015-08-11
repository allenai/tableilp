package org.allenai.ari.solvers.tableilp.params

import com.google.inject.Inject
import com.google.inject.name.Named

/** Various parameters of the ILP model.
  *
  * @param alignmentType whether to use Entailment, WordOverlap, or Word2Vec for alignment scores
  * @param entailmentScoreOffset amount to subtract from raw score returned by entailment service
  * @param mustChooseAnAnswer must select an answer choice (disable for debugging)
  * @param maxTablesToChain maximum number of tables that may be chained together in the solution
  * @param maxRowsPerTable how many rows per table may an inference chain use inference
  * @param useRelaxedVars whether to use relaxed (continuous) variables, where possible
  * @param keyColumnsMustMatch whether at least one KEY column must match when a table is used
  */
class IlpParams @Inject() (
  @Named("model.alignmentType") val alignmentType: String,
  @Named("model.entailmentScoreOffset") val entailmentScoreOffset: Double,
  @Named("model.mustChooseAnAnswer") val mustChooseAnAnswer: Boolean,
  @Named("model.maxTablesToChain") val maxTablesToChain: Int,
  @Named("model.maxRowsPerTable") val maxRowsPerTable: Int,
  @Named("model.useRelaxedVars") val useRelaxedVars: Boolean,
  @Named("model.keyColumnsMustMatch") val keyColumnsMustMatch: Boolean
) {}

/** An object to capture the default ILP model parameters */
object IlpParams {
  val Default = new IlpParams(
    alignmentType = "Entailment",
    entailmentScoreOffset = 0.2d,
    mustChooseAnAnswer = true,
    maxTablesToChain = 4,
    maxRowsPerTable = 1,
    useRelaxedVars = false,
    keyColumnsMustMatch = true
  )
}

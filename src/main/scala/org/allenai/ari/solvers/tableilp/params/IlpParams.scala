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
  */
class IlpParams @Inject() (
  @Named("model.alignmentType") val alignmentType: String,
  @Named("model.entailmentScoreOffset") val entailmentScoreOffset: Double,
  @Named("model.mustChooseAnAnswer") val mustChooseAnAnswer: Boolean,
  @Named("model.maxTablesToChain") val maxTablesToChain: Int,
  @Named("model.maxRowsPerTable") val maxRowsPerTable: Int
) {}

/** An object to capture the default ILP model parameters */
object IlpParams {
  val Default = new IlpParams("Entailment", 0.2d, mustChooseAnAnswer = true, 4, 1)
}

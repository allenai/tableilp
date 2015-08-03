package org.allenai.ari.solvers.tableilp

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
  @Named("model.alignmentType") val alignmentType: String = "Entailment",
  @Named("model.entailmentScoreOffset") val entailmentScoreOffset: Double = 0.2d,
  @Named("model.mustChooseAnAnswer") val mustChooseAnAnswer: Boolean = true,
  @Named("model.maxTablesToChain") val maxTablesToChain: Int = 4,
  @Named("model.maxRowsPerTable") val maxRowsPerTable: Int = 1
) {}

/** An object to capture the default ILP model parameters */
object IlpParams {
  val Default = new IlpParams()
}
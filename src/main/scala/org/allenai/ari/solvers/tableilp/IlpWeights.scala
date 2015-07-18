package org.allenai.ari.solvers.tableilp

import com.google.inject.Inject
import com.google.inject.name.Named

/** Various weights to parameterize the ILP model. */
class IlpWeights @Inject() (
  @Named("weights.minCellCellAlignment") val minCellCellAlignment: Double = 0.2,
  @Named("weights.minCellQConsAlignment") val minCellQConsAlignment: Double = 0.2,
  @Named("weights.minTitleQConsAlignment") val minTitleQConsAlignment: Double = 0.2,
  @Named("weights.minTitleTitleAlignment") val minTitleTitleAlignment: Double = 0.2,
  @Named("weights.activeCellObjCoeff") val activeCellObjCoeff: Double = 0d,
  @Named("weights.activeRowObjCoeff") val activeRowObjCoeff: Double = 0d,
  @Named("weights.activeColObjCoeff") val activeColObjCoeff: Double = 0d,
  @Named("weights.activeTitleObjCoeff") val activeTitleObjCoeff: Double = 0d,
  @Named("weights.activeQConsObjCoeff") val activeQConsObjCoeff: Double = 1d,
  @Named("weights.activeChoiceObjCoeff") val activeChoiceObjCoeff: Double = 100d
) {}

/** An object to default the default weights settings */
object IlpWeights {
  val Default = new IlpWeights()
}
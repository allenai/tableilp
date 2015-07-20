package org.allenai.ari.solvers.tableilp

import com.google.inject.Inject
import com.google.inject.name.Named

/** Various weights to parameterize the ILP model.
  *
  * @param minCellCellAlignment Minimum threshold for considering cell-to-cell alignment
  * @param minCellQConsAlignment Minimum threshold for considering cell-to-question alignment
  * @param minTitleQConsAlignment Minimum threshold for considering title-to-question alignment
  * @param minTitleTitleAlignment Minimum threshold for considering title-to-title alignment
  * @param activeCellObjCoeff How much does an active cell contribute to the objective function
  * @param activeRowObjCoeff How much does an active row contribute to the objective function
  * @param activeColObjCoeff How much does an active column contribute to the objective function
  * @param activeTitleObjCoeff How much does an active title contribute to the objective function
  * @param activeQConsObjCoeff How much does an active qcons contribute to the objective function
  * @param activeChoiceObjCoeff How much does an active choice contribute to the objective function
  */
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

/** An object to capture the default weight settings */
object IlpWeights {
  val Default = new IlpWeights()
}
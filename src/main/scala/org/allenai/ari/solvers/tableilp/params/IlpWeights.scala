package org.allenai.ari.solvers.tableilp.params

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
  * @param activeTableObjCoeff How much does an active table contribute to the objective function
  * @param activeQConsObjCoeff How much does an active qcons contribute to the objective function
  * @param activeChoiceObjCoeff How much does an active choice contribute to the objective function
  */
class IlpWeights @Inject() (
  @Named("weights.minCellCellAlignment") val minCellCellAlignment: Double,
  @Named("weights.minCellQConsAlignment") val minCellQConsAlignment: Double,
  @Named("weights.minTitleQConsAlignment") val minTitleQConsAlignment: Double,
  @Named("weights.minCellQChoiceAlignment") val minCellQChoiceAlignment: Double,
  @Named("weights.minTitleQChoiceAlignment") val minTitleQChoiceAlignment: Double,
  @Named("weights.minTitleTitleAlignment") val minTitleTitleAlignment: Double,
  @Named("weights.activeCellObjCoeff") val activeCellObjCoeff: Double,
  @Named("weights.activeRowObjCoeff") val activeRowObjCoeff: Double,
  @Named("weights.activeColObjCoeff") val activeColObjCoeff: Double,
  @Named("weights.activeTitleObjCoeff") val activeTitleObjCoeff: Double,
  @Named("weights.activeTableObjCoeff") val activeTableObjCoeff: Double,
  @Named("weights.activeQConsObjCoeff") val activeQConsObjCoeff: Double,
  @Named("weights.activeChoiceObjCoeff") val activeChoiceObjCoeff: Double
) {}

/** An object to capture the default weight settings */
object IlpWeights {
  val Default = new IlpWeights(0.2d, 0.2d, 0.2d, 0.2d, 0.2d, 0.2d, 0d, 0d, 0d, 0d, 0d, 1d, 100d)
}

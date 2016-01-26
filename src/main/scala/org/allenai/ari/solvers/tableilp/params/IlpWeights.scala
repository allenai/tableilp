package org.allenai.ari.solvers.tableilp.params

import com.google.inject.Inject
import com.google.inject.name.Named

/** Various weights to parameterize the ILP model.
  *
  * @param minCellCellAlignment Minimum threshold for considering cell-to-cell alignment
  * @param minCellQConsAlignment Minimum threshold for considering cell-to-question alignment
  * @param minTitleQConsAlignment Minimum threshold for considering title-to-question alignment
  * @param minTitleTitleAlignment Minimum threshold for considering title-to-title alignment
  * @param minActiveCellAggrAlignment Minimum total alignment that an active cell must have
  * @param minActiveTitleAggrAlignment Minimum total alignment that an active title must have
  * @param activeCellObjCoeff How much does an active cell contribute to the objective function
  * @param activeColObjCoeff How much does an active column contribute to the objective function
  * @param activeTitleObjCoeff How much does an active title contribute to the objective function
  * @param tableScoreObjCoeff How much does the table matching score contribute to the obj function
  * @param activeQConsObjCoeff How much does an active qcons contribute to the objective function
  * @param activeChoiceObjCoeff How much does an active choice contribute to the objective function
  * @param activeScienceTermBoost Multiplicative scaling factor to use if a science term is active
  * @param whichTermBoost Additive boost for active question terms that follow the word 'which'
  * @param minAlignmentWhichTerm Minimum alignment for boost due to cell match to 'which' terms
  * @param tableUsagePenalty How much to penalize the use of a table
  * @param rowUsagePenalty How much to penalize the use of a row
  * @param interTableAlignmentPenalty How much to penalize alignments across two tables
  * @param maxAlignmentsPerQCons How many external alignments may a question constituent have
  */
class IlpWeights @Inject() (
  @Named("weights.minCellCellAlignment") val minCellCellAlignment: Double,
  @Named("weights.minCellQConsAlignment") val minCellQConsAlignment: Double,
  @Named("weights.minTitleQConsAlignment") val minTitleQConsAlignment: Double,
  @Named("weights.minCellQChoiceAlignment") val minCellQChoiceAlignment: Double,
  @Named("weights.minTitleQChoiceAlignment") val minTitleQChoiceAlignment: Double,
  @Named("weights.minCellQChoiceConsAlignment") val minCellQChoiceConsAlignment: Double,
  @Named("weights.minTitleQChoiceConsAlignment") val minTitleQChoiceConsAlignment: Double,
  @Named("weights.minTitleTitleAlignment") val minTitleTitleAlignment: Double,
  @Named("weights.minActiveCellAggrAlignment") val minActiveCellAggrAlignment: Double,
  @Named("weights.minActiveTitleAggrAlignment") val minActiveTitleAggrAlignment: Double,
  @Named("weights.activeCellObjCoeff") val activeCellObjCoeff: Double,
  @Named("weights.activeColObjCoeff") val activeColObjCoeff: Double,
  @Named("weights.activeTitleObjCoeff") val activeTitleObjCoeff: Double,
  @Named("weights.tableScoreObjCoeff") val tableScoreObjCoeff: Double,
  @Named("weights.activeQConsObjCoeff") val activeQConsObjCoeff: Double,
  @Named("weights.activeChoiceObjCoeff") val activeChoiceObjCoeff: Double,
  @Named("weights.activeScienceTermBoost") val activeScienceTermBoost: Double,
  @Named("weights.whichTermBoost") val whichTermBoost: Double,
  @Named("weights.minAlignmentWhichTerm") val minAlignmentWhichTerm: Double,
  @Named("weights.tableUsagePenalty") val tableUsagePenalty: Double,
  @Named("weights.rowUsagePenalty") val rowUsagePenalty: Double,
  @Named("weights.interTableAlignmentPenalty") val interTableAlignmentPenalty: Double,
  @Named("weights.maxAlignmentsPerQCons") val maxAlignmentsPerQCons: Int,
  @Named("weights.maxAlignmentsPerCell") val maxAlignmentsPerCell: Int,
  @Named("weights.relationMatchCoeff") val relationMatchCoeff: Double,
  @Named("weights.emptyRelationMatchCoeff") val emptyRelationMatchCoeff: Double,
  @Named("weights.noRelationMatchCoeff") val noRelationMatchCoeff: Double

) {}

/** An object to capture the default weight settings */
object IlpWeights {
  val Default = new IlpWeights(
    minCellCellAlignment = 0.2d,
    minCellQConsAlignment = 0.2d,
    minTitleQConsAlignment = 0.2d,
    minCellQChoiceAlignment = 0.2d,
    minTitleQChoiceAlignment = 0.2d,
    minCellQChoiceConsAlignment = 0.4d,
    minTitleQChoiceConsAlignment = 0.4d,
    minTitleTitleAlignment = 0.2d,
    minActiveCellAggrAlignment = 0.1d,
    minActiveTitleAggrAlignment = 0.1d,
    activeCellObjCoeff = 0d,
    activeColObjCoeff = 0d,
    activeTitleObjCoeff = 0d,
    tableScoreObjCoeff = 1d,
    activeQConsObjCoeff = 1d,
    activeChoiceObjCoeff = 100d,
    activeScienceTermBoost = 1d,
    whichTermBoost = 1.5d,
    minAlignmentWhichTerm = 0.6d,
    tableUsagePenalty = 4d,
    rowUsagePenalty = 1d,
    interTableAlignmentPenalty = 1d,
    maxAlignmentsPerQCons = 2,
    maxAlignmentsPerCell = 2,
    relationMatchCoeff = 0.2,
    emptyRelationMatchCoeff = 0.0,
    noRelationMatchCoeff = -5.0
  )
}

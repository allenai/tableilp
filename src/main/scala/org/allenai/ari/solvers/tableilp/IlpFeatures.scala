package org.allenai.ari.solvers.tableilp

import org.allenai.common.Logging

/** A class to build real-valued features for the question and solution, for potential use in a
  * trained solver combination module.
  */
class IlpFeatures(ilpSolution: IlpSolution) extends Logging {

  /** A map from feature names to values */
  val featureMap: Map[String, Double] = buildFeatureMap()

  /** Names of all features */
  lazy val featureNames: Seq[String] = featureMap.keys.toSeq

  /** An internal method to process IlpSolution and build a feature map. */
  private def buildFeatureMap(): Map[String, Double] = {
    // number of active cells for each table
    val numActiveCellsPerTable: Seq[Int] = ilpSolution.tableAlignments.map { table =>
      table.contentAlignments.map(row => row.count(cell => cell.alignmentIds.nonEmpty)).sum
    }

    // number of cell alignments for each table
    val numCellAlignmentsPerTable: Seq[Int] = ilpSolution.tableAlignments.map { table =>
      table.contentAlignments.flatMap(row => row.map(cell => cell.alignmentIds.size)).sum
    }

    // number of active title cells for each table
    val numActiveTitlesPerTable: Seq[Int] = ilpSolution.tableAlignments.map { table =>
      table.titleAlignments.count(title => title.alignmentIds.nonEmpty)
    }

    // number of title cell alignments for each table
    val numTitleAlignmentsPerTable: Seq[Int] = ilpSolution.tableAlignments.map { table =>
      table.titleAlignments.map(title => title.alignmentIds.size).sum
    }

    // total number of tables used in the solution
    val numTablesUsed = numActiveCellsPerTable.count(_ > 0)

    // total number of active table content cells in the solution
    val numActiveCells = numActiveCellsPerTable.sum

    // total number of table content cell alignments/edges used in the solution
    val numCellAlignments = numCellAlignmentsPerTable.sum

    // total number of active table titles in the solution
    val numActiveTitles = numActiveTitlesPerTable.sum

    // total number of table title alignments/edges used in the solution
    val numTitleAlignments = numTitleAlignmentsPerTable.sum

    // total number of active question constituents
    val qConsAlignments = ilpSolution.questionAlignment.qConsAlignments
    val numActiveQCons = qConsAlignments.count(qCons => qCons.alignmentIds.nonEmpty)

    // fraction of question constituents that are active
    val fracActiveQCons = if (qConsAlignments.nonEmpty) {
      numActiveQCons / qConsAlignments.size.toDouble
    } else {
      0d
    }

    // total number of question constituent alignments/edges
    val numQConsAlignments = qConsAlignments.map(qCons => qCons.alignmentIds.size).sum

    // collect all alignment scores to compute their average and minimum
    val allAlignmentScores = ilpSolution.alignmentIdToScore.values.toSeq
    val minAlignmentScore = if (allAlignmentScores.nonEmpty) allAlignmentScores.min else 0d
    val avgAlignmentScore = if (allAlignmentScores.nonEmpty) {
      allAlignmentScores.sum / allAlignmentScores.size
    } else {
      0d
    }

    // collect all question constituent alignment scores to compute their average and minimum
    val allQConsAlignmentScores = qConsAlignments.flatMap(_.alignmentIds).map { alignmentId =>
      ilpSolution.alignmentIdToScore(alignmentId)
    }
    val minQConsAlignmentScore = if (allQConsAlignmentScores.nonEmpty) {
      allQConsAlignmentScores.min
    } else {
      0d
    }
    val avgQConsAlignmentScore = if (allQConsAlignmentScores.nonEmpty) {
      allQConsAlignmentScores.sum / allQConsAlignmentScores.size
    } else {
      0d
    }

    // collect all answer choice alignment scores (there may be multiple edges to one choice)
    val qChoiceAlignments = ilpSolution.questionAlignment.choiceAlignments
    val allQChoiceAlignmentScores = qChoiceAlignments.flatMap(_.alignmentIds).map { alignmentId =>
      ilpSolution.alignmentIdToScore(alignmentId)
    }
    val avgQChoiceAlignmentScore = if (allQChoiceAlignmentScores.nonEmpty) {
      allQChoiceAlignmentScores.sum / allQChoiceAlignmentScores.size
    } else {
      0d
    }

    // collect all computed features into a map
    val localMap: Map[String, Double] = Map(
      "numTablesUsed" -> numTablesUsed,
      "numActiveCells" -> numActiveCells,
      "numCellAlignments" -> numCellAlignments,
      "numActiveTitles" -> numActiveTitles,
      "numTitleAlignments" -> numTitleAlignments,
      "numActiveQCons" -> numActiveQCons,
      "fracActiveQCons" -> fracActiveQCons,
      "numQConsAlignments" -> numQConsAlignments,
      "minAlignmentScore" -> minAlignmentScore,
      "avgAlignmentScore" -> avgAlignmentScore,
      "minQConsAlignmentScore" -> minQConsAlignmentScore,
      "avgQConsAlignmentScore" -> avgQConsAlignmentScore,
      "netQChoiceAlignmentScore" -> allQChoiceAlignmentScores.sum,
      "avgQChoiceAlignmentScore" -> avgQChoiceAlignmentScore,
      "numVariables" -> ilpSolution.problemStats.nVars,
      "numConstraints" -> ilpSolution.problemStats.nCons,
      "numSearchNodes" -> ilpSolution.searchStats.nNodes,
      "numLPIterations" -> ilpSolution.searchStats.nLPIterations,
      "maxSearchDepth" -> ilpSolution.searchStats.maxDepth
    )
    logger.debug("features: " + localMap.toString)

    localMap
  }
}

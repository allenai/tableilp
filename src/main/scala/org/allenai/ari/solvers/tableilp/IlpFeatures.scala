package org.allenai.ari.solvers.tableilp

import org.allenai.common.Logging

/** A class to build real-valued features for the question and solution, for potential use in a
  * trained solver combination module.
  */
class IlpFeatures(ilpSolution: IlpSolution) extends Logging {

  /** A map from feature names to values, obtained by processing IlpSolution */
  val featureMap: Map[String, Double] = {
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

    // collect all computed features into a map; for number of variables, LP iterations, etc., use
    // log of the value shifted by 1 (to account for value = 0); for search nodes, use log base 2.
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
      "logVariables" -> math.log1p(ilpSolution.problemStats.nVars),
      "logConstraints" -> math.log1p(ilpSolution.problemStats.nCons),
      "log2SearchNodes" -> math.log1p(ilpSolution.searchStats.nNodes) / math.log(2),
      "logLPIterations" -> math.log1p(ilpSolution.searchStats.nLPIterations),
      // maxDepth is 0 if the ILP is solved at the root note, and is -1 if solved during presolve()
      "logMaxSearchDepth" -> math.log(ilpSolution.searchStats.maxDepth + 2)
    )
    logger.debug("features: " + localMap.toString)

    localMap
  }

  /** Names of all features; note that this will NOT preserve order from run to run;
    * use LinkedHashMap or ListMap if you want to preserve the order in which features are added
    */
  val featureNames: Iterable[String] = featureMap.keys
}

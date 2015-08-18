package org.allenai.ari.solvers.tableilp

import org.allenai.ari.solvers.common.{ EntailmentService, KeywordTokenizer }
import org.allenai.ari.solvers.tableilp.ilpsolver.{ ScipInterface, ScipParams }
import org.allenai.ari.solvers.tableilp.params._
import org.allenai.common.Logging

object IlpExamples extends Logging {

  // a small test to check alignments in a table
  private def runExample(
    questionChunks: Seq[String],
    entailmentServiceOpt: Option[EntailmentService],
    tokenizer: KeywordTokenizer
  ): Unit = {
    val question = TableQuestionFactory.makeQuestion(questionChunks)
    val tableInterface = new TableInterface(TableParams.Default, tokenizer)
    val numTables = 2
    val tables = tableInterface.allTables.slice(0, numTables)
    val scores = Seq.fill(numTables)(1d)
    val tablesWithScores = tables.zip(scores)
    val scipParams = ScipParams.Default
    val ilpParams = IlpParams.Default
    val weights = IlpWeights.Default
    val alignmentType = if (entailmentServiceOpt.isDefined) "Entailment" else "WordOverlap"
    val aligner = new AlignmentFunction(alignmentType, entailmentServiceOpt,
      ilpParams.entailmentScoreOffset, tokenizer, useRedisCache = false)
    val ilpSolver = new ScipInterface("sampleExample", scipParams)
    val ilpModel = new IlpModel(ilpSolver, tablesWithScores, aligner, ilpParams, weights)
    val allVariables = ilpModel.buildModel(question)
    val vars = allVariables.ilpVars
    ilpSolver.solve()
    ilpSolver.printResult(vars)
    tableInterface.printTableVariables(allVariables)
  }

  def main(args: Array[String]): Unit = {
    val questionChunks1 = Seq("USA", "Brazil")
    val questionChunks2 = Seq("In", "New York State", "the", "shortest", "period",
      "of", "daylight", "occurs", "during", "which", "month")
    runExample(questionChunks1, None, KeywordTokenizer.Default)
  }
}

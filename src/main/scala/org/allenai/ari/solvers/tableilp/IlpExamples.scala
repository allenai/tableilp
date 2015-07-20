package org.allenai.ari.solvers.tableilp

import org.allenai.ari.solvers.common.{ EntailmentService, KeywordTokenizer }
import org.allenai.ari.solvers.tableilp.ilpsolver.{ ScipParams, ScipInterface }
import org.allenai.common.Logging

object IlpExamples extends Logging {

  // a small test to check alignments in a table
  private def runExample(
    questionChunks: Seq[String],
    entailmentServiceOpt: Option[EntailmentService],
    tokenizer: KeywordTokenizer
  ): Unit = {
    val question = TableQuestionFactory.makeQuestion(questionChunks)
    val tableInterface = new TableInterface("src/main/resources/allTables", "", false)
    val tables = tableInterface.allTables.slice(0, 2)
    val alignmentType = if (entailmentServiceOpt.isDefined) "Entailment" else "WordOverlap"
    val aligner = new AlignmentFunction(alignmentType, entailmentServiceOpt, 0.2, tokenizer)
    val ilpSolver = new ScipInterface("sampleExample", ScipParams.Default)
    val weights = IlpWeights.Default
    val ilpModel = new IlpModel(ilpSolver, tables, aligner, weights)
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

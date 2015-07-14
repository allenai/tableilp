package org.allenai.ari.solvers.tableilp

import org.allenai.ari.solvers.common.{ EntailmentService, KeywordTokenizer }
import org.allenai.ari.solvers.tableilp.ilpsolver.ScipInterface
import org.allenai.common.Logging

object IlpExamples extends Logging {

  // just to check the alignment inside one table
  def example1(
    entailmentServiceOpt: Option[EntailmentService],
    tokenizer: KeywordTokenizer
  ): Unit = {
    val questionChunks = Seq("USA", "Brazil")
    val tables = TableInterface.loadTables()
    val alignmentType = if (entailmentServiceOpt.isDefined) {
      "Entailment"
    } else {
      "WordOverlap"
    }
    val aligner = new AlignmentFunction(alignmentType, entailmentServiceOpt, tokenizer)
    val ilpSolver = new ScipInterface("sampleExample")
    val ilpModel = new IlpModel(ilpSolver, tables, aligner)
    val question = QuestionFactory.makeQuestion(questionChunks)
    val allVariables = ilpModel.buildModel(question)
    val vars = allVariables.ilpVars
    ilpSolver.solve()
    ilpSolver.printResult(vars)
    TableInterface.printTableVariables(allVariables)
  }

  def example2(
    entailmentServiceOpt: Option[EntailmentService],
    tokenizer: KeywordTokenizer
  ): Unit = {
    val questionChunks = Seq("In", "New York State", "the", "shortest", "period",
      "of", "daylight", "occurs", "during", "which", "month")
    val tables = TableInterface.loadAllTables()
    val alignmentType = if (entailmentServiceOpt.isDefined) {
      "Entailment"
    } else {
      "WordOverlap"
    }
    val aligner = new AlignmentFunction(alignmentType, entailmentServiceOpt, tokenizer)
    val ilpSolver = new ScipInterface("sampleExample")
    val ilpModel = new IlpModel(ilpSolver, tables, aligner)
    val question = QuestionFactory.makeQuestion(questionChunks)
    val allVariables = ilpModel.buildModel(question)
    ilpSolver.solve()
    val vars = allVariables.ilpVars
    ilpSolver.printResult(vars)
    TableInterface.printTableVariables(allVariables)
  }

  def main(args: Array[String]): Unit = {
    val tokenizer = KeywordTokenizer.Default
    example1(None, tokenizer)
    // example2(None, tokenizer)
  }
}

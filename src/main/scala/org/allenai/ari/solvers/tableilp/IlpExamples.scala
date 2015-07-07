package org.allenai.ari.solvers.tableilp

import org.allenai.ari.solvers.common.EntailmentService
import org.allenai.ari.solvers.tableilp.ilpsolver.ScipInterface
import org.allenai.common.Logging

object IlpExamples extends Logging {

  // just to check the alignment inside one table
  def example1(entailmentServiceOpt: Option[EntailmentService]): Unit = {
    val questionChunks = Seq("USA", "Brazil")
    val tables = TableInterface.loadTables()
    val aligner = new AlignmentFunction(SimilarityType.Entailment, entailmentServiceOpt)
    val ilpSolver = new ScipInterface("sampleExample")
    val ilpModel = new IlpModel(ilpSolver, tables, aligner)
    val question = new Question(questionChunks)
    val allVariables = ilpModel.buildModel(question)
    val vars = allVariables.getIlpVars
    ilpSolver.solve()
    ilpSolver.printResult(vars)
    TableInterface.printTableVariables(allVariables)
  }

  def example2(entailmentServiceOpt: Option[EntailmentService]): Unit = {
    val questionChunks = Seq("In", "New York State", "the", "shortest", "period",
      "of", "daylight", "occurs", "during", "which", "month")
    val tables = TableInterface.loadAllTables()
    val aligner = new AlignmentFunction(SimilarityType.Entailment, entailmentServiceOpt)
    val ilpSolver = new ScipInterface("sampleExample")
    val ilpModel = new IlpModel(ilpSolver, tables, aligner)
    val question = new Question(questionChunks)
    val allVariables = ilpModel.buildModel(question)
    ilpSolver.solve()
    val vars = allVariables.getIlpVars
    ilpSolver.printResult(vars)
    TableInterface.printTableVariables(allVariables)
  }

  def main(args: Array[String]): Unit = {
    example1(None)
    // example2(None)
  }
}

package org.allenai.ari.solvers.tableilp

import org.allenai.ari.solvers.common.KeywordTokenizer
import org.allenai.ari.solvers.tableilp.ilpsolver.{ ScipInterface, ScipParams }
import org.allenai.ari.solvers.tableilp.params.{ IlpParams, IlpWeights, TableParams }
import org.allenai.common.testkit.UnitSpec

/** Test overall functionality of the TableILP solver */
class IlpSolverSpec extends UnitSpec {

  private val tokenizer = KeywordTokenizer.Default
  private val scipParams = new ScipParams(10d, "scip.log", messagehdlrQuiet = true, 0)
  private val ilpParams = IlpParams.Default
  private val weights = IlpWeights.Default
  private val tableParams = TableParams.Default
  private val tableInterface = new TableInterface(tableParams, tokenizer)
  private val alignmentType = "WordOverlap"
  private val aligner = new AlignmentFunction(alignmentType, None, ilpParams.entailmentScoreOffset,
    tokenizer, useRedisCache = false)

  /** solve a question and return selected answer choice with score */
  private def solve(questionChunks: Seq[String], choices: Seq[String],
    tableIds: Seq[(Int, Double)]): (Int, Double) = {
    val questionIlp = TableQuestionFactory.makeQuestion(questionChunks, choices)
    val ilpSolver = new ScipInterface("sampleExample", scipParams)
    val ilpModel = new IlpModel(ilpSolver, aligner, ilpParams, weights, tableInterface, tableIds)
    val allVariables = ilpModel.buildModel(questionIlp)
    val vars = allVariables.ilpVars
    ilpSolver.solve()
    val tablesUsed = tableIds.map { case (t, _) => tableInterface.allTables(t) }
    val ilpSolution = IlpSolutionFactory.makeIlpSolution(allVariables, ilpSolver,
      questionIlp, tablesUsed, fullTablesInIlpSolution = false)
    val (choice, score) = IlpSolutionFactory.getBestChoice(allVariables, ilpSolver)
    ilpSolver.free()
    (choice, score)
  }

  "ILP solver" should "solve the question correctly in" in {
    // "Which characteristic can a human offspring inherit? (A) borken leg  (B) blue eyes"
    val questionText = "Which characteristic can a human offspring inherit?"
    val questionChunks = tokenizer.keywordTokenize(questionText)
    val choices = Seq("broken leg", "blue eyes")
    val tableIds = tableInterface.getTableIdsForQuestion(questionText).take(1)
    val (choice, score) = solve(questionChunks, choices, tableIds)
    choice should be(1)
    assert(score === 11.6945 +- 1e-4)
  }
}
package org.allenai.ari.solvers.tableilp

import org.allenai.common.testkit.UnitSpec

/** Test functionality of alignments, etc., in IlpSolution */
class IlpSolutionSpec extends UnitSpec {
  "ilpSolution" should "not crash when creating a solution object" in {
    val ilpSolution = IlpSolutionFactory.makeRandomIlpSolution
    ilpSolution.bestChoice should be(1)
    ilpSolution.tableAlignments.size should be(2)
    ilpSolution.questionAlignment.choiceAlignments.size should be(4)
  }
}
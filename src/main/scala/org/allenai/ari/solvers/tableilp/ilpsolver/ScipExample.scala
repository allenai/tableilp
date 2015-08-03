package org.allenai.ari.solvers.tableilp.ilpsolver

import org.allenai.common.Logging

/** An example of invoking the SCIP ILP solver from the Scala project.
  */
object ScipExample extends Logging {

  /** Main method to run an ILP model from the command line. */
  def main(args: Array[String]): Unit = {
    val scipSolver = new ScipInterface("example", ScipParams.Default)
    val varsOfInterest = buildModel(scipSolver)
    scipSolver.solve()
    scipSolver.printResult(varsOfInterest)
  }

  /** Build a simple ILP model: x0 + 2*x1 <= 2, objective function: - x0 - x1.
    *
    * @param scipSolver a ScipInterface object
    * @return a seq of (a subset of) variables of interest whose values may be queried later
    */
  private def buildModel(scipSolver: ScipInterface): Seq[Long] = {
    // create a seq of variables
    val nvars = 2

    // create binary variables
    val vars = for {
      i <- 0 until nvars
      name = s"x$i"
      objCoeff = -1d
      x = scipSolver.createBinaryVar(name, objCoeff)
    } yield {
      logger.debug(s"created variable $name with pointer $x")
      scipSolver.addVar(x)
      x
    }
    val varsOfInterest = new Array[Long](1)
    varsOfInterest(0) = vars(1) // add x1 to the list of variables of interest to return

    // create coefficients for the constraint
    val coeffs = (0 until nvars).map(_ + 1d)

    // add a linear constraint
    scipSolver.addConsBasicLinear("test", vars, coeffs, Some(0d), Some(2d))

    // return vars of interest
    varsOfInterest
  }
}

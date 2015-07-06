package org.allenai.ari.solvers.tableilp.ilpsolver

import org.allenai.common.Logging

/** An example of invoking the SCIP ILP solver from the Scala project.
  */
object ScipExample extends Logging {

  /** Main method to run an ILP model from the command line. */
  def main(args: Array[String]): Unit = {
    val scipSolver = new ScipInterface("example")
    val vars = buildModel(scipSolver)
    scipSolver.solve()
    scipSolver.printResult(vars)
  }

  /** Build a simple ILP model: x0 + 2*x1 <= 2, objective function: - x0 - x1.
    *
    * @param scipSolver a ScipInterface object
    * @return an array of (a subset of) variables of interest whose values may be queried later
    */
  private def buildModel(scipSolver: ScipInterface): Array[Long] = {
    // create an array of variables
    val nvars = 2
    val varsOfInterest = new Array[Long](nvars)

    // create binary variables
    val vars = for {
      i <- 0 until nvars
      name = s"x$i"
      objCoeff = -1d
      x = scipSolver.createBinaryVar(name, objCoeff)
    } yield {
      logger.debug(s"created variable $name with pointer $x")
      scipSolver.addVar(x)
      varsOfInterest(i) = x // keep track of x as a variable whose value may be queried later
      x
    }

    // create coefficients for the constraint
    val coeffs = (0 until nvars).map(_ + 1d)

    // create a linear constraint
    val consLinear = scipSolver.createConsBasicLinear("test-linear", vars, coeffs, 0d, 2d)

    // add constraints to problem
    scipSolver.addCons(consLinear)

    if (internalLogger.isDebugEnabled) {
      val coeffs = scipSolver.getValsLinear(consLinear)
      logger.debug("Constraint coefficients:\n\t" + coeffs.mkString("\n\t"))
    }

    // release constraints
    scipSolver.releaseCons(consLinear)

    // return vars of interest
    varsOfInterest
  }
}

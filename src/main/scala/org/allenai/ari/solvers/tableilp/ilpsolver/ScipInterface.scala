package org.allenai.ari.solvers.tableilp.ilpsolver

import org.allenai.common.Logging

import de.zib.jscip.nativ.jni._

/** Various relevant status values after an ILP is solved */
sealed trait IlpStatus
case object IlpStatusUnknown extends IlpStatus { override def toString = "Unknown" }
case object IlpStatusOptimal extends IlpStatus { override def toString = "Optimal" }
case object IlpStatusFeasible extends IlpStatus { override def toString = "Feasible" }
case object IlpStatusInfeasible extends IlpStatus { override def toString = "Infeasible" }

/** This is a generic interface to the SCIP ILP solver providing a number of common initialization
  * steps and access to the SCIP environment. This class is NOT guaranteed to be thread-safe!
  */
class ScipInterface(probName: String, scipParams: ScipParams) extends Logging {
  // Min and max values to use when defining the model
  // TODO(ashish33) check how to access SCIP's built-in SCIP_REAL_MAX, etc.
  private val ScipMin = -1e+20
  private val ScipMax = 1e+20

  // initialization: load JNI library
  logger.debug("Java library path = " + System.getProperty("java.library.path"))
  JniScipLibraryLoader.loadLibrary()

  // initialization: create various handlers in the SCIP environment
  // create the SCIP environment
  private val env: JniScip = new JniScip

  // create the SCIP variable environment
  private val envVar: JniScipVar = new JniScipVar

  // create SCIP set packing constraint environment
  private val envConsSetppc = new JniScipConsSetppc

  // create the SCIP linear constraint environment
  private val envConsLinear = new JniScipConsLinear

  // initialization: create a SCIP instance
  private val scip: Long = env.create

  // initialization: set various parameters
  env.printVersion(scip, scipParams.printVersion)
  env.setMessagehdlrQuiet(scip, scipParams.messagehdlrQuiet)
  env.setMessagehdlrLogfile(scip, scipParams.logFile)
  env.includeDefaultPlugins(scip) // include default plugins of SCIP
  env.setRealParam(scip, "limits/time", scipParams.timeLimit) // set SCIP's overall time limit

  // initialization: create empty problem tied to the given problem name
  env.createProbBasic(scip, probName)

  /** set objective function as minimization */
  def setAsMinimization(): Unit = env.setObjsense(scip, JniScipObjsense.SCIP_OBJSENSE_MINIMIZE)

  /** set objective function as maximization */
  def setAsMaximization(): Unit = env.setObjsense(scip, JniScipObjsense.SCIP_OBJSENSE_MAXIMIZE)

  /** create a binary variable */
  def createBinaryVar(name: String, obj: Double): Long = {
    env.createVarBasic(scip, name, 0, 1, obj, JniScipVartype.SCIP_VARTYPE_BINARY)
  }

  /** create an integer variable */
  def createIntegerVar(name: String, lb: Double, ub: Double, objCoeff: Double): Long = {
    env.createVarBasic(scip, name, lb, ub, objCoeff, JniScipVartype.SCIP_VARTYPE_INTEGER)
  }

  /** create a continuous variable */
  def createContinuousVar(name: String, lb: Double, ub: Double, objCoeff: Double): Long = {
    env.createVarBasic(scip, name, lb, ub, objCoeff, JniScipVartype.SCIP_VARTYPE_CONTINUOUS)
  }

  /** add variable to the environment */
  def addVar(x: Long): Unit = env.addVar(scip, x)

  /** add constraint to the environment */
  def addCons(c: Long): Unit = env.addCons(scip, c)

  /** release constraint from the environment */
  def releaseCons(c: Long): Unit = env.releaseCons(scip, c)

  /** get the name of a variable */
  def varGetName(l: Long): String = envVar.varGetName(l)

  /** get number of variables in the original ILP */
  def getNOrigVars: Int = env.getNOrigVars(scip)

  /** get number of constraints in the original ILP */
  def getNOrigConss: Int = env.getNOrigConss(scip)

  /** get number of active variables, after presolve */
  def getNVars: Int = env.getNVars(scip)

  /** get number of active variables, after presolve */
  def getNConss: Int = env.getNConss(scip)

  /** get solution status */
  def getStatus: IlpStatus = {
    env.getStatus(scip) match {
      case JniScipStatus.SCIP_STATUS_OPTIMAL => IlpStatusOptimal
      case JniScipStatus.SCIP_STATUS_INFEASIBLE => IlpStatusInfeasible
      case _ if getBestSol != 0 => IlpStatusFeasible // best solution isn't null
      case _ => IlpStatusUnknown
    }
  }

  /** check whether a solution has been found */
  private def IlpStatusFeasibleOrOptimal = Seq(IlpStatusOptimal, IlpStatusFeasible)
  def hasSolution: Boolean = IlpStatusFeasibleOrOptimal.contains(getStatus)

  /** get pointer to the best solution found */
  def getBestSol: Long = env.getBestSol(scip)

  /** get objective value (primal bound) */
  def getPrimalbound: Double = env.getPrimalbound(scip)

  /** get objective value (dual bound) */
  def getDualbound: Double = env.getDualbound(scip)

  /** get optimality gap */
  def getGap: Double = env.getGap(scip)

  /** get solution values */
  def getSolVals(vars: Iterable[Long]): Iterable[Double] = {
    env.getSolVals(scip, getBestSol, vars.size, vars.toArray)
  }

  /** get one solution value */
  def getSolVal(variable: Long): Double = env.getSolVal(scip, getBestSol, variable)

  /** get time spent in presolve routine */
  def getPresolvingTime: Double = env.getPresolvingTime(scip)

  /** get time spent in main solve routine */
  def getSolvingTime: Double = env.getSolvingTime(scip)

  /** get total time spent by SCIP */
  def getTotalTime: Double = env.getTotalTime(scip)

  /** get the number of search nodes explored during branch and bound */
  def getNNodes: Long = env.getNNodes(scip)

  /** get the number of simplex iterations used when solving LP relaxations */
  def getNLPIterations: Long = env.getNLPIterations(scip)

  /** get the maximal depth of nodes explored during branch and bound */
  def getMaxDepth: Int = env.getMaxDepth(scip)

  /** Adds a constraint to SCIP and "release" it */
  def addReleaseCons(cons: Long): Unit = {
    env.addCons(scip, cons)
    env.releaseCons(scip, cons)
  }

  /** Sets the lower bound for a variable */
  def chgVarLb(x: Long, bound: Double): Unit = env.chgVarLb(scip, x, bound)

  /** If triggered, imposes a lower bound for a variable; trigger is binary variable */
  def chgVarLb(x: Long, bound: Double, trigger: Long): Unit = {
    // TODO Find a way to require that 'trigger' is a binary variable
    addConsBasicLinear("VarLb", Seq(x), Seq(1d), Some(bound), None, trigger)
  }

  /** Sets the upper bound for a variable */
  def chgVarUb(x: Long, bound: Double): Unit = env.chgVarUb(scip, x, bound)

  /** If triggered, imposes a upper bound for a variable; trigger is binary variable */
  def chgVarUb(x: Long, bound: Double, trigger: Long): Unit = {
    addConsBasicLinear("VarUb", Seq(x), Seq(1d), None, Some(bound), trigger)
  }

  /** Creates and captures a linear constraint in its most basic version; all constraint flags are
    * set to their basic value as explained for the method SCIPcreateConsLinear(); all flags can
    * be set via SCIPsetConsFLAGNAME methods in scip.h
    *
    * @see SCIPcreateConsLinear() for information about the basic constraint flag configuration
    *
    * @param name                  name of constraint
    * @param vars                  seq with variables of constraint entries
    * @param coeffs                seq with coefficients of constraint entries
    * @param lhsOpt                left hand side of constraint, optional
    * @param rhsOpt                right hand side of constraint, optional
    */
  def createConsBasicLinear(name: String, vars: Seq[Long], coeffs: Seq[Double],
    lhsOpt: Option[Double], rhsOpt: Option[Double]): Long = {
    val lhs = if (lhsOpt.isDefined) lhsOpt.get else ScipMin
    val rhs = if (rhsOpt.isDefined) rhsOpt.get else ScipMax
    envConsLinear.createConsBasicLinear(scip, name, vars.length, vars.toArray, coeffs.toArray,
      lhs, rhs)
  }

  /** Calls createConsBasicLinear and adds the constraint to the solver */
  def addConsBasicLinear(name: String, vars: Seq[Long], coeffs: Seq[Double],
    lhsOpt: Option[Double], rhsOpt: Option[Double]): Unit = {
    addReleaseCons(createConsBasicLinear(name, vars, coeffs, lhsOpt, rhsOpt))
  }

  /** If triggered, imposes a basic linear constraint to the solver; trigger is binary variable */
  def addConsBasicLinear(name: String, vars: Seq[Long], coeffs: Seq[Double],
    lhsOpt: Option[Double], rhsOpt: Option[Double], trigger: Long): Unit = {
    val largeDbl = 1000000d // a very large value, compared to sum_i var[i] * coeffs[i]
    addReleaseCons(createConsBasicLinear(name, vars :+ trigger, coeffs :+ largeDbl, lhsOpt, None))
    addReleaseCons(createConsBasicLinear(name, vars :+ trigger, coeffs :+ -largeDbl, None, rhsOpt))
  }

  /** Adds coefficient to a linear constraint (if it is not zero)
    *
    * @param cons                  constraint data
    * @param x                     variable of constraint entry
    * @param coeff                 coefficient of constraint entry
    */
  def addCoefLinear(cons: Long, x: Long, coeff: Double): Unit = {
    envConsLinear.addCoefLinear(scip, cons, x, coeff)
  }

  /** Gets the seq of coefficient values in the linear constraint; the user must not modify
    * this seq!
    *
    * @param cons                  constraint data
    */
  def getValsLinear(cons: Long): Seq[Double] = envConsLinear.getValsLinear(scip, cons)

  /** Creates and captures a basic Set Partitioning constraint, sum_i x_i = 1, emulating C++ API's
    * createConsBasicSetpack constraint which is not provided in the Java API.
    *
    * @param name                  name of constraint
    * @param vars                  seq with variables of constraint entries
    */
  def createConsBasicSetpart(name: String, vars: Seq[Long]): Long = {
    envConsSetppc.createConsSetpart(scip, name, vars.length, vars.toArray,
      true, true, true, true, true, false, false, false, false, false)
  }

  /** Calls createConsBasicSetpart and adds the constraint to the solver */
  def addConsBasicSetpart(name: String, vars: Seq[Long]): Unit = {
    addReleaseCons(createConsBasicSetpart(name, vars))
  }

  /** If triggered, imposes a set partitioning constraint, sum_i x_i = 1; trigger is binary var */
  def addConsBasicSetpart(name: String, vars: Seq[Long], trigger: Long): Unit = {
    addConsBasicLinear(name, vars, Seq.fill(vars.size)(1d), Some(1d), Some(1d), trigger)
  }

  /** Creates and captures a basic Set Packing constraint, sum_i x_i <= 1, emulating C++ API's
    * createConsBasicSetpack constraint which is not provided in the Java API.
    *
    * @param name                  name of constraint
    * @param vars                  seq with variables of constraint entries
    */
  def createConsBasicSetpack(name: String, vars: Seq[Long]): Long = {
    envConsSetppc.createConsSetpack(scip, name, vars.length, vars.toArray,
      true, true, true, true, true, false, false, false, false, false)
  }

  /** Calls createConsBasicSetpack and adds the constraint to the solver */
  def addConsBasicSetpack(name: String, vars: Seq[Long]): Unit = {
    addReleaseCons(createConsBasicSetpack(name, vars))
  }

  /** If triggered, imposes a set packing constraint, sum_i x_i <= 1; trigger is binary variable */
  def addConsBasicSetpack(name: String, vars: Seq[Long], trigger: Long): Unit = {
    addConsBasicLinear(name, vars, Seq.fill(vars.size)(1d), None, Some(1d), trigger)
  }

  /** Creates and captures a basic Set covering constraint, sum_i x_i >= 1, emulating C++ API's
    * createConsBasicSetpack constraint which is not provided in the Java API.
    *
    * @param name                  name of constraint
    * @param vars                  seq with variables of constraint entries
    */
  def createConsBasicSetcover(name: String, vars: Seq[Long]): Long = {
    envConsSetppc.createConsSetcover(scip, name, vars.length, vars.toArray,
      true, true, true, true, true, false, false, false, false, false)
  }

  /** Calls createConsBasicSetcover and adds the constraint to the solver */
  def addConsBasicSetcover(name: String, vars: Seq[Long]): Unit = {
    addReleaseCons(createConsBasicSetcover(name, vars))
  }

  /** If triggered, imposes a set covering constraint, sum_i x_i >= 1; trigger is binary variable */
  def addConsBasicSetcover(name: String, vars: Seq[Long], trigger: Long): Unit = {
    addConsBasicLinear(name, vars, Seq.fill(vars.size)(1d), Some(1d), None, trigger)
  }

  /** Adds coefficient in set partitioning / packing / covering constraint
    *
    * @param cons                  constraint data
    * @param x                     variable to add to the constraint
    */
  def addCoefSetppc(cons: Long, x: Long): Unit = envConsSetppc.addCoefSetppc(scip, cons, x)

  /** Adds the constraint x <= y + c */
  def addConsXLeqYPlusC(name: String, x: Long, y: Long, c: Double): Unit = {
    addConsBasicLinear(name, Seq(x, y), Seq(1d, -1d), None, Some(c))
  }

  /** If triggered, imposes the constraint x <= y + c; trigger is binary variable */
  def addConsXLeqYPlusC(name: String, x: Long, y: Long, c: Double, trigger: Long): Unit = {
    addConsBasicLinear(name, Seq(x, y), Seq(1d, -1d), None, Some(c), trigger)
  }

  /** Adds the constraint x <= y */
  def addConsXLeqY(name: String, x: Long, y: Long): Unit = {
    addConsXLeqYPlusC(name, x, y, 0d)
  }

  /** If triggered, imposes the constraint x <= y; trigger is binary variable */
  def addConsXLeqY(name: String, x: Long, y: Long, trigger: Long): Unit = {
    addConsXLeqYPlusC(name, x, y, 0d, trigger)
  }

  /** Adds the constraint x = y + c */
  def addConsXEqYPlusC(name: String, x: Long, y: Long, c: Double): Unit = {
    addConsBasicLinear(name, Seq(x, y), Seq(1d, -1d), Some(c), Some(c))
  }

  /** If triggered, imposes the constraint x = y + c; trigger is binary variable */
  def addConsXEqYPlusC(name: String, x: Long, y: Long, c: Double, trigger: Long): Unit = {
    addConsBasicLinear(name, Seq(x, y), Seq(1d, -1d), Some(c), Some(c), trigger)
  }

  /** Adds the constraint x = y */
  def addConsXEqY(name: String, x: Long, y: Long): Unit = {
    addConsXEqYPlusC(name, x, y, 0d)
  }

  /** If triggered, imposes the constraint x = y; trigger is binary variable */
  def addConsXEqY(name: String, x: Long, y: Long, trigger: Long): Unit = {
    addConsXEqYPlusC(name, x, y, 0d, trigger)
  }

  /** Adds the constraint sum(X) >= k * y */
  def addConsSumImpliesY(name: String, X: Seq[Long], y: Long, k: Double): Unit = {
    val vars = X :+ y
    val coeffs = Seq.fill(X.size)(1d) :+ (-k)
    addConsBasicLinear(name, vars, coeffs, Some(0d), None)
  }

  /** If triggered, imposes the constraint sum(X) >= k * y; trigger is binary variable */
  def addConsSumImpliesY(name: String, X: Seq[Long], y: Long, k: Double, trigger: Long): Unit = {
    val vars = X :+ y
    val coeffs = Seq.fill(X.size)(1d) :+ (-k)
    addConsBasicLinear(name, vars, coeffs, Some(0d), None, trigger)
  }

  /** Solve the ILP model and report the result */
  def solve(): Unit = {
    env.solve(scip)
    logger.info(s"Solution status: $getStatus")
    logger.info(s"Objective value: $getPrimalbound")
  }

  /** Print result of the call to solve(), along with solution values of vars */
  def printResult(vars: Seq[Long]): Unit = {
    // retrieve best solution found so far
    if (getStatus == IlpStatusOptimal || getStatus == IlpStatusFeasible) {
      val values = getSolVals(vars)
      val solution = vars.zip(values) map { case (x, v) => varGetName(x) + " : " + v }
      logger.info("Solution found:\n\t" + solution.mkString("\n\t"))
    }
  }
}

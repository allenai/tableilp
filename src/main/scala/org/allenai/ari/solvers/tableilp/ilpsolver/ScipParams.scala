package org.allenai.ari.solvers.tableilp.ilpsolver

import com.google.inject.Inject
import com.google.inject.name.Named

/** Various parameters for the SCIP ILP solver
  *
  * @param timeLimit Overall time limit for SCIP in seconds once it starts solving the model
  * @param logFile Log file where SCIP output is stored for debugging purposes
  * @param messagehdlrQuiet Whether to have SCIP's message handler be quiet or write to log file
  * @param printVersion Integer, indicating whether to print SCIP's version to log
  */
class ScipParams @Inject() (
  @Named("scip.timeLimit") val timeLimit: Double,
  @Named("scip.logFile") val logFile: String,
  @Named("scip.messagehdlrQuiet") val messagehdlrQuiet: Boolean,
  @Named("scip.printVersion") val printVersion: Int
) {}

/** An object to capture the default SCIP parameters */
object ScipParams {
  val Default = new ScipParams(180d, "scip.log", messagehdlrQuiet = false, 0)
}

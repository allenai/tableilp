package org.allenai.ari.solvers.tableilp

import org.allenai.common.Logging

import com.google.inject.Inject
import com.google.inject.name.Named

/** A class for storing and processing multiple tables.
  *
  * @param tablesFolder Name of the folder from which to read tables
  * @param questionToTablesCache Name of the cheat sheet mapping question to relevant tables
  * @param useCachedTablesForQuestion Whether to use the above cheat sheet
  */
class TableInterface @Inject() (
    @Named("tablesFolder") tablesFolder: String,
    @Named("questionToTablesCache") questionToTablesCache: String,
    @Named("useCachedTablesForQuestion") useCachedTablesForQuestion: Boolean
) extends Logging {

  /** config: a cheat sheet mapping training questions from question to tables */
  private lazy val questionToTables = new Table(questionToTablesCache).contentMatrix

  /** All tables loaded from CSV files */
  val allTables = {
    logger.info(s"Loading tables from folder $tablesFolder")
    val files = new java.io.File(tablesFolder).listFiles.filter(_.getName.endsWith(".csv")).toSeq
    val tables = files.map(file => new Table(file.getAbsolutePath))
    logger.debug(s"${tables.size} tables loaded from files:\n" + files.mkString("\n"))
    if (internalLogger.isTraceEnabled) tables.foreach(t => logger.trace(t.titleRow.mkString(",")))
    tables
  }

  /** Get a subset of tables relevant for a given question */
  def getTablesForQuestion(question: String): Seq[Table] = {
    val tables = if (useCachedTablesForQuestion) {
      getCachedTablesForQuestion(question)
    } else {
      getMatchingTablesForQuestion(question)
    }
    logger.debug(s"using ${tables.size} tables:\n" +
      tables.map("table: " + _.titleRow.mkString(",") + "\n"))
    assert(tables.size <= 4, "Only up to 4 tables supported")
    tables
  }

  /** Get a subset of tables relevant for a given question, by looking up a cheat sheet */
  private def getCachedTablesForQuestion(question: String): Seq[Table] = {
    val questionToTablesOpt = questionToTables.find(_(1) == question) orElse
      questionToTables.find(_(1).trim == question.trim)
    val tablesOpt = questionToTablesOpt map { qToTables =>
      qToTables(2).split('-').map(idx => allTables(idx.toInt)).toSeq
    } orElse {
      Some(Seq.empty)
    }
    tablesOpt.get
  }

  /** Get a subset of tables relevant for a given question, by using salience, etc. */
  private def getMatchingTablesForQuestion(question: String): Seq[Table] = {
    // TODO(daniel) implement
    logger.warn("Not yet properly implemented!")
    allTables.slice(0, 4)
  }

  /** Print all variables relevant to tables */
  def printTableVariables(allVariables: AllVariables): Unit = {
    if (internalLogger.isDebugEnabled) {
      // intra table
      logger.debug("Intra Table Variables = ")
      logger.debug("\n\t" + allVariables.intraTableVariables.mkString("\n\t"))

      // inter table
      logger.debug("Intra Table Variables = ")
      logger.debug("\n\t" + allVariables.interTableVariables.mkString("\n\t"))

      // question table
      logger.debug("Question Table Variables = ")
      logger.debug("\n\t" + allVariables.questionTableVariables.mkString("\n\t"))
    }
  }
}

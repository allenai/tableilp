package org.allenai.ari.solvers.tableilp

import org.allenai.ari.solvers.common.KeywordTokenizer
import org.allenai.common.Logging

import scala.math._

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
    @Named("useCachedTablesForQuestion") useCachedTablesForQuestion: Boolean,
    tokenizer: KeywordTokenizer
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

  /** tables after some normalization */
  val normalizedTables = normalizeTables(tokenizer, allTables)

  /** td idf maps */
  val (tfMap, idfMap) = calculateAllTFIDFScores()

  /** Get a subset of tables relevant for a given question */
  def getTablesForQuestion(question: String): Seq[Table] = {
    val tables = if (useCachedTablesForQuestion) {
      getCachedTablesForQuestion(question)
    } else {
      getRankedTablesForQuestion(question)
    }
    logger.debug(s"using ${tables.size} tables:\n" +
      tables.map("table: " + _.titleRow.mkString(",") + "\n"))
    assert(tables.size <= 4, "Only up to 4 tables supported")
    tables
  }

  /** Get a subset of tables relevant for a given question, by looking up a cheat sheet
    * Here we removed table 18, which contains a large dump of WordNet
    * The index 18 gets converted to 15. Here is the list of tables and their indices
    * https://docs.google.com/spreadsheets/d/1YTPuMOX8EB4YnCrKnQXOV99zZC8QYSkfbCDN3rsfcYM/edit#gid=506523592
    */
  private def getCachedTablesForQuestion(question: String): Seq[Table] = {
    val questionToTablesOpt = questionToTables.find(_(1) == question) orElse
      questionToTables.find(_(1).trim == question.trim)
    val tablesOpt = questionToTablesOpt map { qToTables =>
      qToTables(2).split('-').filterNot(_.toInt == 15).map(idx => allTables(idx.toInt)).toSeq
    } orElse {
      Some(Seq.empty)
    }
    tablesOpt.get
  }

  /** Get a subset of tables relevant for a given question, by using salience, etc. */
  def getRankedTablesForQuestion(question: String): Seq[Table] = {
    val topN = 5
    val scoreIndexPairs = (1 until allTables.length).map { tableIdx =>
      (tableIdx, tfidfTableScore(tokenizer, tableIdx, question))
    }
    scoreIndexPairs.sortBy(-_._2).slice(0, topN).map { case (idx, score) => allTables(idx) }
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

  private def calculateAllTFIDFScores(): (Map[(String, Int), Double], Map[String, Double]) = {
    val numberOfTables = normalizedTables.size.toDouble
    val allTableTokens = normalizedTables.flatten.flatten.flatten.toSet
    val eachTableTokens = normalizedTables.map { _.flatten.flatten }

    val tfMap = (for {
      tableIdx <- normalizedTables.indices
      word <- allTableTokens
    } yield {
      val tfcount = eachTableTokens(tableIdx).count(_ == word).toDouble
      ((word, tableIdx), if (tfcount == 0.0) { 0.0 } else { 1.0 + log10(tfcount) })
    }).toMap

    val idfMap = (for {
      word <- allTableTokens
    } yield {
      val dfcount = eachTableTokens.count { table => table.contains(word) }.toDouble
      (word, if (dfcount == 0.0) { 0.0 } else { log10(numberOfTables / dfcount) })
    }).toMap
    (tfMap, idfMap)
  }

  private def normalizeTables(tokenizer: KeywordTokenizer, tables: Seq[Table]): Seq[Seq[Seq[Seq[String]]]] = {
    val normalizedTables = tables.map { table =>
      val content = table.contentMatrix :+ table.titleRow
      content.map(row => row.map(tokenizer.stemmedKeywordTokenize))
    }
    normalizedTables
  }

  private def tfidfTableScore(tokenizer: KeywordTokenizer, tableIdx: Int, questionRaw: String): Double = {
    val table = normalizedTables(tableIdx)
    val qaTokens = tokenizer.stemmedKeywordTokenize(questionRaw.toLowerCase)
    val currentTableTokens = table.flatten.flatten
    val commonTokenSet = currentTableTokens.toSet.intersect(qaTokens.toSet).toVector
    val currentTableScore = commonTokenSet.map(token => tfMap.getOrElse((token, tableIdx), 0.0) *
      idfMap.getOrElse(token, 0.0)).sum
    val qaOverlapScore = qaTokens.map(token => if (commonTokenSet.contains(token)) 1 else 0).sum
      .toDouble / qaTokens.length.toDouble
    val tableOverlapScore = currentTableTokens.map { token =>
      if (commonTokenSet.contains(token)) 1 else 0
    }.sum.toDouble / currentTableTokens.length.toDouble
    currentTableScore * qaOverlapScore * tableOverlapScore
  }
}

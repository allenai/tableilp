package org.allenai.ari.solvers.tableilp

import org.allenai.ari.solvers.common.KeywordTokenizer
import org.allenai.common.Logging

import com.google.inject.Inject
import com.google.inject.name.Named

import scala.math._

/** A class for storing and processing multiple tables.
  *
  * @param folder Name of the folder from which to read tables
  * @param questionToTablesCache Name of the cheat sheet mapping question to relevant tables
  * @param useCachedTablesForQuestion Whether to use the above cheat sheet
  * @param ignoreList A comma-separated list of able IDs to ignore
  * @param tokenizer A keyword tokenizer
  */
class TableInterface @Inject() (
    @Named("tables.folder") folder: String,
    @Named("tables.questionToTablesCache") questionToTablesCache: String,
    @Named("tables.useCachedTablesForQuestion") useCachedTablesForQuestion: Boolean,
    @Named("tables.ignoreList") ignoreList: String,
    tokenizer: KeywordTokenizer
) extends Logging {
  /** All tables loaded from CSV files */
  val allTables = {
    logger.info(s"Loading tables from folder $folder")
    val files = new java.io.File(folder).listFiles.filter(_.getName.endsWith(".csv")).toSeq
    val tables = files.map(file => new Table(file.getAbsolutePath, tokenizer))
    logger.debug(s"${tables.size} tables loaded:\n" +
      files.zipWithIndex.map { case (file, idx) => s"\ntable $idx = $file" })
    if (internalLogger.isTraceEnabled) tables.foreach(t => logger.trace(t.titleRow.mkString(",")))
    tables
  }

  /** a sequence of table indices to ignore */
  private val tablesToIgnore = ignoreList.split(',').map(_.toInt).toSeq
  logger.info("Ignoring table IDs " + tablesToIgnore.mkString(","))

  if (useCachedTablesForQuestion) {
    logger.info(s"Using CACHED tables for questions from $questionToTablesCache")
  } else {
    logger.info("Using RANKED tables for questions")
  }

  /** a cheat sheet mapping training questions from question to tables */
  private lazy val questionToTables = new Table(questionToTablesCache, tokenizer).contentMatrix

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

  /** Get a subset of tables relevant for a given question, by looking up a cheat sheet. */
  private def getCachedTablesForQuestion(question: String): Seq[Table] = {
    val questionToTablesOpt = questionToTables.find(_(1) == question) orElse
      questionToTables.find(_(1).trim == question.trim)
    val tablesOpt = questionToTablesOpt map { qToTables =>
      qToTables(2).split('-').map(_.toInt).filterNot(tablesToIgnore.contains).map(allTables).toSeq
    } orElse {
      Some(Seq.empty)
    }
    tablesOpt.get
  }

  /** Get a subset of tables relevant for a given question, by using salience, etc. */
  private def getRankedTablesForQuestion(question: String): Seq[Table] = {
    val withThreshold = false
    val thresholdValue = 0.17333
    val topN = 1
    // ignore table 18 (which has index 15)
    val scoreIndexPairs = allTables.indices.filterNot(tablesToIgnore.contains).map { tableIdx =>
      (tableIdx, tfidfTableScore(tokenizer, tableIdx, question))
    }
    (if (!withThreshold) {
      scoreIndexPairs.sortBy(-_._2).slice(0, topN)
    } else {
      scoreIndexPairs.filter(_._2 > thresholdValue)
    }).map { case (idx, score) => allTables(idx) }
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
    val numberOfTables = allTables.size
    val eachTableTokens = allTables.map(tab => tab.fullContentNormalized.flatten.flatMap(_.values))
    val allTableTokens = eachTableTokens.flatMap(toks => toks)

    val tfMap = (for {
      tableIdx <- allTables.indices
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

  private def tfidfTableScore(tokenizer: KeywordTokenizer, tableIdx: Int, questionRaw: String): Double = {
    val table = allTables(tableIdx).fullContentNormalized
    val qaTokens = tokenizer.stemmedKeywordTokenize(questionRaw.toLowerCase)
    val currentTableTokens = table.flatten.flatMap(_.values)
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

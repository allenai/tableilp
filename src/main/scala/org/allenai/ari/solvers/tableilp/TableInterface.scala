package org.allenai.ari.solvers.tableilp

import org.allenai.ari.solvers.common.KeywordTokenizer
import org.allenai.ari.solvers.tableilp.params.TableParams
import org.allenai.common.Logging

import com.google.inject.Inject

/** A class for storing and processing multiple tables.
  *
  * @param params Various knowledge table related parameters
  * @param tokenizer A keyword tokenizer
  */
class TableInterface @Inject() (
    params: TableParams,
    tokenizer: KeywordTokenizer
) extends Logging {
  /** All tables loaded from CSV files */
  val allTables = {
    logger.info(s"Loading tables from folder ${params.folder}")
    val files = new java.io.File(params.folder).listFiles.filter(_.getName.endsWith(".csv")).sorted
      .toSeq
    val tables = files.map(file => new Table(file.getAbsolutePath, tokenizer))
    logger.debug(s"${tables.size} tables loaded:\n" +
      files.zipWithIndex.map { case (file, idx) => s"\ntable $idx = $file" })
    if (internalLogger.isTraceEnabled) tables.foreach(t => logger.trace(t.titleRow.mkString(",")))
    tables
  }

  /** a sequence of table indices to ignore */
  logger.info("Ignoring table IDs " + params.ignoreList.toString())

  if (params.useCachedTablesForQuestion) {
    logger.info(s"Using CACHED tables for questions from ${params.questionToTablesCache}")
  } else {
    logger.info("Using RANKED tables for questions")
  }

  /** a cheat sheet mapping training questions from question to tables */
  private lazy val questionToTables = new Table(params.questionToTablesCache, tokenizer)
    .contentMatrix

  /** td idf maps */
  val (tfMap, idfMap) = calculateAllTFIDFScores()

  /** Get a subset of tables (with scores) relevant for a given question */
  def getTablesForQuestion(question: String): Seq[(Table, Double)] = {
    val tablesWithScores = if (params.useCachedTablesForQuestion) {
      getCachedTablesForQuestion(question)
    } else {
      getRankedTablesForQuestion(question)
    }
    logger.debug(s"using ${tablesWithScores.size} tables:\n" +
      tablesWithScores.map("table: " + _._1.titleRow.mkString(",") + "\n"))
    assert(
      tablesWithScores.size <= params.maxTablesPerQuestion,
      s"Only max ${params.maxTablesPerQuestion} tables supported"
    )
    tablesWithScores
  }

  private def sep = "-".r
  /** Get a subset of tables relevant for a given question, by looking up a cheat sheet. */
  private def getCachedTablesForQuestion(question: String): Seq[(Table, Double)] = {
    val questionToTablesOpt = questionToTables.find(_(1) == question) orElse
      questionToTables.find(_(1).trim == question.trim)
    val tablesOpt = questionToTablesOpt map { qTables =>
      sep.split(qTables(2)).map(_.toInt).diff(params.ignoreList).map(allTables).toSeq
    } orElse {
      Some(Seq.empty)
    }
    val tables = tablesOpt.get
    val defaultScores = Seq.fill(tables.size)(1d)
    tables.zip(defaultScores)
  }

  /** Get a subset of tables relevant for a given question, by using salience, etc. */
  private def getRankedTablesForQuestion(question: String): Seq[(Table, Double)] = {
    val scoreIndexPairs = allTables.indices.diff(params.ignoreList).map { tableIdx =>
      (tableIdx, tfidfTableScore(tokenizer, tableIdx, question))
    }
    (if (!params.useRankThreshold) {
      scoreIndexPairs.sortBy(-_._2).slice(0, params.maxTablesPerQuestion)
    } else {
      scoreIndexPairs.filter(_._2 > params.rankThreshold)
    }).map { case (idx, score) => (allTables(idx), score) }
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
      ((word, tableIdx), if (tfcount == 0.0) { 0.0 } else { 1.0 + math.log10(tfcount) })
    }).toMap

    val idfMap = (for {
      word <- allTableTokens
    } yield {
      val dfcount = eachTableTokens.count { table => table.contains(word) }.toDouble
      (word, if (dfcount == 0.0) { 0.0 } else { math.log10(numberOfTables / dfcount) })
    }).toMap
    (tfMap, idfMap)
  }

  private def tfidfTableScore(tokenizer: KeywordTokenizer, tableIdx: Int,
    questionRaw: String): Double = {
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

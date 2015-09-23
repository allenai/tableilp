package org.allenai.ari.solvers.tableilp

import org.allenai.ari.solvers.common.KeywordTokenizer
import org.allenai.ari.solvers.tableilp.params.TableParams
import org.allenai.common.Logging

import au.com.bytecode.opencsv.CSVReader
import com.google.inject.Inject

import java.io.{ File, FileReader }

import scala.collection.JavaConverters._

/** A structure to store which two columns in two tables are allowed to be joined/aligned.
  *
  * @param table1Name first table
  * @param col1Idx column index in first table
  * @param table2Name second table
  * @param col2Idx column index in second table
  */
case class AllowedTitleAlignment(table1Name: String, col1Idx: Int, table2Name: String, col2Idx: Int)

/** A class for storing and processing multiple tables.
  *
  * @param params Various knowledge table related parameters
  * @param tokenizer A keyword tokenizer
  */
class TableInterface @Inject() (params: TableParams, tokenizer: KeywordTokenizer) extends Logging {

  /** All tables loaded from CSV files */
  val allTables: IndexedSeq[Table] = {
    logger.info(s"Loading tables from folder ${params.folder}")
    val files = new File(params.folder).listFiles.filter(_.getName.endsWith(".csv")).sorted.toSeq
    files.map(file => new Table(file.getName, new FileReader(file), tokenizer))
  }.toIndexedSeq
  logger.debug(s"${allTables.size} tables loaded")
  private val allTableNames = allTables.map(_.fileName)
  logger.debug("tables with internal IDs:\n\t" + allTableNames.zipWithIndex.toString())
  if (internalLogger.isTraceEnabled) allTables.foreach(t => logger.trace(t.titleRow.mkString(",")))

  /** a sequence of table indices to ignore */
  logger.info("Ignoring table IDs " + params.ignoreList.toString())

  if (params.useCachedTablesForQuestion) {
    logger.info(s"Using CACHED tables for questions from ${params.questionToTablesCache}")
  } else {
    logger.info("Using RANKED tables for questions")
  }

  /** titles that are allowed to be aligned */
  val allowedTitleAlignments: Seq[AllowedTitleAlignment] = {
    if (params.allowedColumnAlignmentsFile.isEmpty) Seq.empty else readAllowedTitleAlignments()
  }

  /** a cheat sheet mapping training questions from question to tables; build only if/when needed;
    * format: question number (ignore), question text, hyphen-separated table IDs, other info
    */
  private lazy val questionToTablesMap: Map[String, Seq[Int]] = {
    val mapData: Seq[Seq[String]] = new Table(
      params.questionToTablesCache,
      Utils.getResourceAsReader(params.questionToTablesCache),
      tokenizer
    ).contentMatrix
    val hyphenSep = "-".r
    mapData.map { row =>
      val trimmedQuestion = row(1).trim
      val tableIds = hyphenSep.split(row(2)).map(_.toInt).toSeq
      trimmedQuestion -> tableIds.diff(params.ignoreList)
    }.toMap
  }

  /** td idf maps */
  val (tfMap, idfMap) = calculateAllTFIDFScores()

  /** Get a subset of tables (with scores) relevant for a given question */
  def getTableIdsForQuestion(question: String): Seq[(Int, Double)] = {
    val tableIdsWithScores = if (params.useCachedTablesForQuestion) {
      getCachedTableIdsForQuestion(question)
    } else {
      getRankedTableIdsForQuestion(question)
    }
    logger.debug(s"using ${tableIdsWithScores.size} tables:\n" +
      tableIdsWithScores.map {
        case (t, s) => s"table $t: " + allTables(t).titleRow.mkString(",") + "\n"
      })
    tableIdsWithScores
  }

  /** Get a subset of tables relevant for a given question, by looking up a cheat sheet. */
  private def getCachedTableIdsForQuestion(question: String): Seq[(Int, Double)] = {
    val tableIds: Seq[Int] = questionToTablesMap.getOrElse(question.trim, Seq.empty)
    // TODO: consider having cached table matching scores or using the tfidfTableScore() heuristic
    val defaultScores = Seq.fill(tableIds.size)(1d)
    tableIds.zip(defaultScores)
  }

  /** Get a subset of tables relevant for a given question, by using salience, etc. */
  private def getRankedTableIdsForQuestion(question: String): Seq[(Int, Double)] = {
    val scoreIndexPairs = allTables.indices.diff(params.ignoreList).map { tableIdx =>
      (tableIdx, tfidfTableScore(tokenizer, tableIdx, question))
    }
    if (!params.useRankThreshold) {
      scoreIndexPairs.sortBy(-_._2).slice(0, params.maxTablesPerQuestion)
    } else {
      scoreIndexPairs.filter(_._2 > params.rankThreshold)
    }
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
      tfcount = eachTableTokens(tableIdx).count(_ == word)
      score = if (tfcount == 0) 0d else 1d + math.log10(tfcount)
    } yield ((word, tableIdx), score)).toMap

    val idfMap = (for {
      word <- allTableTokens
      dfcount = eachTableTokens.count { table => table.contains(word) }
      score = if (dfcount == 0) 0d else math.log10(numberOfTables / dfcount.toDouble)
    } yield (word, score)).toMap
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

  private def stripComments(inputString: String): String = {
    // remove all comments of the form "// blah blah"
    val commentRegex = "//[\\S\\s]+?.*".r
    commentRegex.replaceAllIn(inputString, "")
  }

  private def readAllowedTitleAlignments(): Seq[AllowedTitleAlignment] = {
    logger.info("Reading list of titles that are allowed to be aligned")
    val reader = new CSVReader(Utils.getResourceAsReader(params.allowedColumnAlignmentsFile))
    val fullContents: Seq[Seq[String]] = reader.readAll.asScala.map(_.toSeq)
    val fullContentsWithoutCommentsAndEmptyLines = for {
      row <- fullContents
      // TODO(tushar) figure out why row.nonEmpty doesn't work below
      if row.size > 1
      if !row.head.startsWith("//")
    } yield row.map(stripComments(_).trim)
    val allowedAlignments = fullContentsWithoutCommentsAndEmptyLines map {
      case Seq(table1Name, col1IdxStr, table2Name, col2IdxStr) => {
        Seq(table1Name, table2Name).foreach { name =>
          if (!allTableNames.contains(name)) {
            throw new IllegalArgumentException(s"table $name does not exist")
          }
        }
        AllowedTitleAlignment(table1Name, col1IdxStr.toInt, table2Name, col2IdxStr.toInt)
      }
      case _ => {
        throw new IllegalArgumentException(s"Error processing ${params.allowedColumnAlignmentsFile}")
      }
    }
    logger.debug(allowedAlignments.toString())
    allowedAlignments
  }
}

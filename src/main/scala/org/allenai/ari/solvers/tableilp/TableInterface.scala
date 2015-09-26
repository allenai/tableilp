package org.allenai.ari.solvers.tableilp

import org.allenai.ari.solvers.common.KeywordTokenizer
import org.allenai.ari.solvers.tableilp.params.TableParams
import org.allenai.common.Logging
import org.allenai.datastore.Datastore

import au.com.bytecode.opencsv.CSVReader
import com.google.inject.Inject
import com.typesafe.config.Config

import java.io.{ File, FileReader }

import scala.collection.JavaConverters._

/** A structure to store which two columns in two tables are allowed to be joined/aligned.
  *
  * @param table1Name first table
  * @param col1Idx column index in first table
  * @param table2Name second table
  * @param col2Idx column index in second table
  */
case class AllowedColumnAlignment(
  table1Name: String,
  col1Idx: Int,
  table2Name: String,
  col2Idx: Int
)

/** A class for storing and processing multiple tables.
  *
  * @param params various knowledge table related parameters
  * @param tokenizer a keyword tokenizer
  */
class TableInterface @Inject() (params: TableParams, tokenizer: KeywordTokenizer) extends Logging {

  /** All tables loaded from CSV files */
  val allTables: IndexedSeq[Table] = {
    val folder = if (params.useLocal) {
      // read tables from the specified local folder
      logger.info(s"Loading tables from local folder ${params.localFolder}")
      new File(params.localFolder)
    } else {
      // read tables from the specified Datastore folder
      val config: Config = params.datastoreFolderConfig
      val datastoreName = config.getString("datastore")
      val group = config.getString("group")
      val name = config.getString("name")
      val version = config.getInt("version")
      logger.info(s"Loading from $datastoreName datastore, $group/$name-v$version")
      Datastore(datastoreName).directoryPath(group, name, version).toFile
    }
    val files = folder.listFiles.filter(_.getName.endsWith(".csv")).sorted.toSeq
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

  /** pairs of columns (in two tables) that are allowed to be aligned */
  val allowedColumnAlignments: Seq[AllowedColumnAlignment] = {
    if (params.allowedColumnAlignmentsFile.isEmpty) Seq.empty else readAllowedColumnAlignments()
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

  /** Compute TF-IDF scores for all words in relevant tables */
  private def calculateAllTFIDFScores(): (Map[(String, Int), Double], Map[String, Double]) = {
    val numberOfTables: Int = allTables.size
    val perTableTokens: IndexedSeq[IndexedSeq[String]] = allTables.map { table =>
      table.fullContentTokenized.flatten.flatMap(_.values)
    }

    // collect all distinct words
    val allTokensWithDupes: IndexedSeq[String] = perTableTokens.flatten
    val allTableTokens: IndexedSeq[String] = allTokensWithDupes.distinct
    logger.debug(s"tables have ${allTokensWithDupes.size} tokens (${allTableTokens.size} distinct)")
    // turn perTableTokens into a set for fast "contains" check
    val perTableTokenSets: IndexedSeq[Set[String]] = perTableTokens.map(tokens => tokens.toSet)
    // precompute the number of times each word appears in each table
    val perTableTokenCounts: IndexedSeq[Map[String, Int]] = perTableTokens.map {
      tokens => tokens.groupBy(identity).mapValues(_.size)
    }

    val tfMap: Map[(String, Int), Double] = (for {
      tableIdx <- allTables.indices
      word <- allTableTokens
      tfcount <- perTableTokenCounts(tableIdx).get(word)
      score = 1d + math.log10(tfcount)
    } yield ((word, tableIdx), score)).toMap

    val idfMap: Map[String, Double] = (for {
      word <- allTableTokens
      dfcount = perTableTokenSets.count { tokenSet => tokenSet.contains(word) }
      if dfcount > 0
      score = math.log10(numberOfTables / dfcount.toDouble)
    } yield (word, score)).toMap

    (tfMap, idfMap)
  }

  /** Compute TF-IDF score for a question with respect to a given table */
  private def tfidfTableScore(
    tokenizer: KeywordTokenizer,
    tableIdx: Int,
    questionRaw: String
  ): Double = {
    val tableTokens = allTables(tableIdx).fullContentTokenized.flatten.flatMap(_.values)
    val qaTokens = tokenizer.stemmedKeywordTokenize(questionRaw.toLowerCase)
    val commonTokenSet = tableTokens.toSet.intersect(qaTokens.toSet)

    val tableScore = (for {
      token <- commonTokenSet.toSeq // toSeq ensures yield doesn't create a subset of {0,1}
      tfScore <- tfMap.get((token, tableIdx))
      idfScore <- idfMap.get(token)
    } yield tfScore * idfScore).sum

    val qaOverlapScore = qaTokens.count(commonTokenSet.contains) / qaTokens.size.toDouble
    val tableOverlapScore = tableTokens.count(commonTokenSet.contains) / tableTokens.size.toDouble

    tableScore * qaOverlapScore * tableOverlapScore
  }

  private def stripComments(inputString: String): String = {
    // remove all comments of the form "// blah blah"
    val commentRegex = "//[\\S\\s]+?.*".r
    commentRegex.replaceAllIn(inputString, "")
  }

  private def readAllowedColumnAlignments(): Seq[AllowedColumnAlignment] = {
    logger.info("Reading list of titles that are allowed to be aligned")
    val csvReader = new CSVReader(Utils.getResourceAsReader(params.allowedColumnAlignmentsFile))
    val fullContents: Seq[Seq[String]] = csvReader.readAll.asScala.map(_.toSeq)
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
        AllowedColumnAlignment(table1Name, col1IdxStr.toInt, table2Name, col2IdxStr.toInt)
      }
      case _ => {
        throw new IllegalStateException(s"Error processing ${params.allowedColumnAlignmentsFile}")
      }
    }
    logger.debug(allowedAlignments.toString())
    allowedAlignments
  }
}

package org.allenai.ari.solvers.tableilp

import org.allenai.ari.models.tables.{ DatastoreExport, Table => DatastoreTable }
import org.allenai.ari.solvers.common.KeywordTokenizer
import org.allenai.ari.solvers.tableilp.params.TableParams
import org.allenai.common.{ Logging, Resource }

import au.com.bytecode.opencsv.CSVReader
import com.google.inject.Inject
import spray.json._

import scala.collection.JavaConverters._
import scala.io.Source
import scala.util.matching.Regex

import java.io.{ File, FileReader }

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

/** A structure to store the relation schema of a table as binary relations between the columns in
  * the table.
  * @param tableName name of the table
  * @param col1Idx column index of argument 1 of the relation
  * @param col2Idx column index of argument 2 of the relation
  * @param relation name of the relation
  */
case class InterColumnRelation(
  tableName: String,
  col1Idx: Int,
  col2Idx: Int,
  relation: String
)

/** A structure to store lexical patterns for relations */
case class RelationPattern(
  pattern: Regex,
  isFlipped: Boolean
)

object RelationPattern {
  def apply(string: String): RelationPattern = {
    // Argument order is flipped, if pattern ends with -1
    val isFlipped = string.endsWith("-1")
    val pattern = string.stripSuffix("-1").r
    RelationPattern(pattern, isFlipped)
  }
}

/** A simple structure to capture a table ID along with a score and a subset of rows.
  * @param id table ID
  * @param score a double, typically capturing its relevance for a question
  * @param rowIds a subset of rows
  */
case class TableSelection(
  id: Int,
  score: Double,
  rowIds: Seq[Int]
)

/** A class for storing and processing multiple tables.
  *
  * @param params various knowledge table related parameters
  * @param tokenizer a keyword tokenizer
  */
class TableInterface @Inject() (params: TableParams, tokenizer: KeywordTokenizer) extends Logging {

  private val ignoreList = {
    if (params.useTablestoreFormat) params.ignoreListTablestore else params.ignoreList
  }

  val allTables: IndexedSeq[Table] = {
    if (params.useTablestoreFormat) {
      val datastoreTables = if (params.useLocal) {
        logger.info(s"Loading tables from local tablestore folder ${params.localTablestoreFile}")
        val file = new File(params.localTablestoreFile)
        val dataString = Resource.using(Source.fromFile(file))(_.getLines().mkString("\n"))
        import spray.json.DefaultJsonProtocol._
        dataString.parseJson.convertTo[IndexedSeq[DatastoreTable]]
      } else {
        logger.info("Loading tables from Tablestore")
        val dataString = Resource.using(
          Utils.getDatastoreFileAsSource(params.datastoreTablestoreConfig)
        ) { input => input.getLines().mkString("\n") }
        val datastoreExport = dataString.parseJson.convertTo[DatastoreExport]
        // Note: Tablestore tables currently come in the reverse order of IDs
        datastoreExport.tables
      }
      for {
        table <- datastoreTables
        if !ignoreList.contains(table.metadata.id.get)
      } yield new TableWithMetadata(table, tokenizer)
    } else {
      val folder = if (params.useLocal) {
        // read tables from the specified local folder
        logger.info(s"Loading csv tables from local folder ${params.localFolder}")
        new File(params.localFolder)
      } else {
        // read tables from the specified Datastore folder
        logger.info("Loading CSV tables from Datastore")
        Utils.getDatastoreDirectoryAsFolder(params.datastoreFolderConfig)
      }
      val files = folder.listFiles.filter(_.getName.endsWith(".csv")).sorted.toSeq
      val csvTables = files.map(file => {
        new Table(file.getName, getCSVContentFromFile(file), tokenizer)
      })
      // Filter out the CSV tables based on their index
      csvTables.zipWithIndex.filterNot(tableWithIdx => ignoreList.contains(tableWithIdx._2)).
        map(_._1).toIndexedSeq
    }
  }
  logger.debug(s"${allTables.size} tables loaded")

  private val allTableNames = allTables.map(_.fileName)
  private val tableNamesToIdx = allTableNames.zipWithIndex.toMap
  logger.debug("tables IDs to internal IDs:\n\t" + tableNamesToIdx.toSeq.sortBy(_._1).toString)
  if (internalLogger.isTraceEnabled) allTables.foreach(t => logger.trace(t.titleRow.mkString(",")))

  /** a sequence of table indices to ignore */
  logger.info("Ignoring table IDs " + ignoreList.toString)

  if (params.useCachedTablesForQuestion) {
    logger.info(s"Using CACHED tables for questions from ${params.questionToTablesCache}")
  } else {
    logger.info("Using RANKED tables for questions")
  }

  /** pairs of columns (in two tables) that are allowed to be aligned */
  val allowedColumnAlignments: Seq[AllowedColumnAlignment] = readAllowedColumnAlignments()

  /** a map from a table to all other tables that it is allowed to align with */
  private val allowedTableAlignments: Map[Int, Seq[Int]] = {
    val allowedTablePairs = allowedColumnAlignments.flatMap { ca =>
      Seq((ca.table1Name, ca.table2Name), (ca.table2Name, ca.table1Name))
    }
    Utils.toMapUsingGroupByFirst(allowedTablePairs).map {
      case (tableName, otherTableNames) => {
        tableNamesToIdx(tableName) -> otherTableNames.map(tableNamesToIdx)
      }
    }
  }

  /** Read the relations between the columns in a table. **/
  val allowedRelations: Seq[InterColumnRelation] = readAllowedRelations()

  /** Read the regex patterns for the relations described in allowedRelations **/
  val relationToRepresentation: Map[String, Seq[RelationPattern]] =
    readRelationRepresentations(params.relationRepresentationFile)

  /** a cheat sheet mapping training questions from question to tables; build only if/when needed;
    * format: question number (ignore), question text, hyphen-separated table IDs, other info
    */
  private lazy val questionToTablesMap: Map[String, Seq[Int]] = {
    val mapData: Seq[Seq[String]] = new Table(
      params.questionToTablesCache,
      getCSVContentFromResource(params.questionToTablesCache),
      tokenizer
    ).contentMatrix
    val hyphenSep = "-".r
    mapData.map { row =>
      val trimmedQuestion = row(1).trim
      val tableIds = hyphenSep.split(row(2)).map(_.toInt).toSeq
      trimmedQuestion -> tableIds.diff(ignoreList)
    }.toMap
  }

  /** td idf maps */
  private val (tfMap, idfMap) = calculateAllTFIDFScores()

  /** Given a question, compute a sequence of tables, each with a score and a subset of rows that
    * are most relevant to the question.
    */
  def getTablesForQuestion(question: String): IndexedSeq[TableSelection] = {
    val tableSelections = if (params.useCachedTablesForQuestion) {
      getCachedTablesForQuestion(question)
    } else {
      getRankedTablesForQuestion(question)
    }
    logger.debug(s"using ${tableSelections.size} tables:\n" +
      tableSelections.map {
        case TableSelection(id, score, rowIds) => {
          s"\ttable id: ${allTables(id).metadataOpt.flatMap(_.id).getOrElse(id)} " +
            s"(score $score, selected ${rowIds.size} rows}) : " +
            allTables(id).titleRow.mkString("|").replace('\n', ' ') // some headers have newlines!
        }
      }.mkString("\n"))
    tableSelections
  }

  /** Get a subset of tables relevant for a given question, by looking up a cheat sheet. */
  private def getCachedTablesForQuestion(question: String): IndexedSeq[TableSelection] = {
    val tableIds: Seq[Int] = questionToTablesMap.getOrElse(question.trim, Seq.empty)
    val questionTokens = tokenizer.stemmedKeywordTokenize(question.toLowerCase)
    // TODO: consider having cached table matching scores or using the tfidfTableScore() heuristic.
    // Currently using a default score of 1.
    val tableSelections = tableIds.map { tableIdx =>
      val score = 1d
      val rowIds = getRankedRowsForQuestion(questionTokens, tableIdx)
      TableSelection(tableIdx, score, rowIds)
    }
    tableSelections.toIndexedSeq
  }

  /** Get a selection of tables relevant for a given question, using quick TF-IDF computation. */
  private def getRankedTablesForQuestion(question: String): IndexedSeq[TableSelection] = {
    // score all tables using tf-idf
    val scoreTables = allTables.indices.map { tableIdx =>
      (tableIdx, tfidfTableScore(tokenizer, tableIdx, question))
    }
    // identify a few top scoring tables
    val topScoredTables = if (!params.useRankThreshold) {
      // Note: use stable sort for repeatability, using tableIdx to break ties
      scoreTables.sortBy {
        case (tableIdx, score) => (-score, tableIdx)
      }.take(params.maxTablesPerQuestion)
    } else {
      scoreTables.filter(_._2 > params.rankThreshold)
    }
    // if desired, add "intermediate" tables that can link two selected tables
    val (selectedTables, connectingTables) = if (params.includeConnectingTables) {
      val topScoredTableIds = topScoredTables.map(_._1)
      // first get all linked tables
      val linkedTables: IndexedSeq[Int] = topScoredTableIds.flatMap { ts =>
        allowedTableAlignments.getOrElse(ts, Seq.empty)
      }
      val tableToScore: Map[Int, Double] = scoreTables.toMap

      // now find those that are linked to at least two topScoredTables
      val connectingTables: Iterable[Int] = linkedTables.groupBy(identity).collect {
        case (t, occurrences) if occurrences.size > 1 && tableToScore.contains(t) &&
          !topScoredTableIds.contains(t) => t
      }
      // attach scores to these tables
      val scoredConnectingTables = connectingTables.map(t => (t, tableToScore(t)))
      // include with original top scoring tables
      (topScoredTables, scoredConnectingTables)
    } else {
      (topScoredTables, Seq.empty)
    }
    // identify most promising rows within each table, turn into a TableSelection
    val questionTokens = tokenizer.stemmedKeywordTokenize(question.toLowerCase)
    val selectedTableRows = selectedTables.map {
      case (tableIdx, score) => {
        val topRowIds = getRankedRowsForQuestion(questionTokens, tableIdx)
        TableSelection(tableIdx, score, topRowIds)
      }
    }
    val connectedTableRows = connectingTables.map(idxScore =>
      TableSelection(idxScore._1, idxScore._2, allTables(idxScore._1).rowIndices))
    selectedTableRows ++ connectedTableRows
  }

  /** Get top scoring rows from a given table for a given tokenized question */
  private def getRankedRowsForQuestion(questionTokens: Seq[String], tableIdx: Int): Seq[Int] = {
    val table = allTables(tableIdx)
    val tokenizedTableRows = table.fullContentTokenized.tail
    val rowIdsWithScores = tokenizedTableRows.zipWithIndex.map {
      case (tokenizedRow, rowIdx) => {
        // score for a row = fraction of row tokens that overlap with question tokens
        val rowTokens = tokenizedRow.flatMap(_.values)
        val rowScore = if (rowTokens.isEmpty) {
          0d
        } else {
          questionTokens.intersect(rowTokens).size.toDouble / rowTokens.size
        }
        (rowIdx, rowScore)
      }
    }
    // sort (row,score) pairs by score, take the top K, project down to row IDs
    // Note: use stable sort for repeatability, using rowIdx to break ties
    rowIdsWithScores.sortBy {
      case (rowIdx, rowScore) => (-rowScore, rowIdx)
    }.take(params.maxRowsPerTable).map(_._1)
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
    // precompute the number of times each word appears in each table;
    // note: the counts will always be strictly positive (no zero entries)
    val perTableTokenCounts: IndexedSeq[Map[String, Int]] = perTableTokens.map { tokens =>
      tokens.groupBy(identity).mapValues(_.size)
    }

    val tfMap: Map[(String, Int), Double] = (for {
      tableIdx <- allTables.indices
      word <- allTableTokens
      // tfcount will always be strictly positive
      tfcount <- perTableTokenCounts(tableIdx).get(word)
      score = 1d + math.log10(tfcount)
    } yield ((word, tableIdx), score)).toMap

    val idfMap: Map[String, Double] = (for {
      word <- allTableTokens
      // dfcount will always be strictly positive
      dfcount = perTableTokenSets.count { tokenSet => tokenSet.contains(word) }
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

  /** remove all comments of the form "// blah blah" */
  private def stripComments(inputString: String): String = {
    val commentRegex = "//[\\S\\s]+?.*".r
    commentRegex.replaceAllIn(inputString, "")
  }

  /** read allowed column alignments (across pairs of tables) from a file */
  private def readAllowedColumnAlignments(): Seq[AllowedColumnAlignment] = {
    val alignmentsFile = {
      if (params.useTablestoreFormat) {
        params.allowedTablestoreColumnAlignmentsFile
      } else {
        params.allowedColumnAlignmentsFile
      }
    }

    if (alignmentsFile.isEmpty) {
      Seq.empty
    } else {
      logger.info("Reading list of titles that are allowed to be aligned")
      val fullContents: Seq[Seq[String]] = getCSVContentFromResource(alignmentsFile)
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
          require(table1Name != table2Name, "Table indices must be different")
          AllowedColumnAlignment(table1Name, col1IdxStr.toInt, table2Name, col2IdxStr.toInt)
        }
        case _ => {
          throw new IllegalStateException(s"Error processing ${params.allowedColumnAlignmentsFile}")
        }
      }
      logger.debug(allowedAlignments.toString)
      allowedAlignments
    }
  }

  /** read relations between pairs of columns in a table */
  private def readAllowedRelations(): Seq[InterColumnRelation] = {
    val file = if (params.useTablestoreFormat) {
      params.columnRelationsTablestoreFile
    } else {
      params.columnRelationsFile
    }
    if (file.isEmpty) {
      Seq.empty
    } else {
      val fullContents: Seq[Seq[String]] = getCSVContentFromResource(file)
      fullContents.flatMap {
        line =>
          if (line.size > 1 && !line.head.startsWith("//")) {
            assert(line.size == 4, s"Expected four columns in ${line.mkString(",")}")
            if (allTableNames.contains(line(0))) {
              Some(InterColumnRelation(line(0), line(1).toInt, line(2).toInt, line(3)))
            } else {
              None
            }
          } else {
            None
          }
      }
    }
  }

  /** read how various relations may be represented lexically */
  private def readRelationRepresentations(file: String): Map[String, Seq[RelationPattern]] = {
    if (file.isEmpty) {
      Map.empty
    } else {
      val fullContents: Seq[Seq[String]] = getCSVContentFromResource(file)
      val predicateRepresentations = fullContents.flatMap { line =>
        if (line.size > 1 && !line.head.startsWith("//")) {
          assert(line.size >= 2, s"Expected at least two columns in ${line.mkString(",")}")
          Some((line(0), RelationPattern(line(1))))
        } else {
          None
        }
      }
      Utils.toMapUsingGroupByFirst(predicateRepresentations)
    }
  }

  /** utility function to read CSV resource file into a sequence of sequence of strings */
  private def getCSVContentFromResource(file: String): Seq[Seq[String]] = {
    val csvReader = new CSVReader(Utils.getResourceAsReader(file))
    Resource.using(csvReader)(_.readAll.asScala.map(_.toSeq))
  }

  /** utility function to read CSV file into a sequence of sequence of strings */
  private def getCSVContentFromFile(file: File): Seq[Seq[String]] = {
    val csvReader = new CSVReader(new FileReader(file))
    Resource.using(csvReader)(_.readAll.asScala.map(_.toSeq))
  }
}

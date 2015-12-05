package org.allenai.ari.solvers.tableilp

import org.allenai.ari.models.tables.{ Table => DatastoreTable }
import org.allenai.ari.solvers.common.KeywordTokenizer
import org.allenai.ari.solvers.tableilp.params.TableParams
import org.allenai.common.Logging
import org.allenai.datastore.Datastore

import au.com.bytecode.opencsv.CSVReader
import com.google.inject.Inject
import com.typesafe.config.Config
import spray.json._

import scala.collection.JavaConverters._
import scala.io.Source
import scala.util.matching.Regex

import java.io.{ File, FileReader, Reader }

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

// TODO(ericgribkoff) Copied from tables/, refactor out to models/
case class DatastoreExport(tables: IndexedSeq[DatastoreTable], description: String)

object DatastoreExport {

  import spray.json.DefaultJsonProtocol._

  implicit val datastoreJsonFormat = jsonFormat2(DatastoreExport.apply)
}

/** A class for storing and processing multiple tables.
  *
  * @param params various knowledge table related parameters
  * @param tokenizer a keyword tokenizer
  */
class TableInterface @Inject() (params: TableParams, tokenizer: KeywordTokenizer) extends Logging {

  private val ignoreList = {
    if (params.useTablestoreFormat) {
      params.ignoreListTablestore
    } else {
      params.ignoreList
    }
  }

  private def getFullContentsFromCsvFile(reader: Reader): Seq[Seq[String]] = {
    val csvReader = new CSVReader(reader)
    csvReader.readAll.asScala.map(_.toSeq)
  }

  val allTables: IndexedSeq[Table] = {
    if (params.useTablestoreFormat) {
      val datastoreTables = if (params.useLocal) {
        logger.info(s"Loading tables from local tablestore folder ${params.localTablestoreFile}")
        val file = new File(params.localTablestoreFile)
        val dataString = Source.fromFile(file).getLines().mkString("\n")
        import spray.json.DefaultJsonProtocol._
        dataString.parseJson.convertTo[IndexedSeq[DatastoreTable]]
      } else {
        val config: Config = params.datastoreTablestoreConfig
        val datastoreName = config.getString("datastore")
        val group = config.getString("group")
        val name = config.getString("name")
        val version = config.getInt("version")
        logger.info(
          s"Loading tables from tablestore $datastoreName datastore,$group/$name-v$version"
        )
        val file = Datastore(datastoreName).filePath(group, name, version).toFile
        val dataString = Source.fromFile(file).getLines().mkString("\n")
        val datastoreExport = dataString.parseJson.convertTo[DatastoreExport]
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
        val config: Config = params.datastoreFolderConfig
        val datastoreName = config.getString("datastore")
        val group = config.getString("group")
        val name = config.getString("name")
        val version = config.getInt("version")
        logger.info(s"Loading csv from $datastoreName datastore, $group/$name-v$version")
        Datastore(datastoreName).directoryPath(group, name, version).toFile
      }
      val files = folder.listFiles.filter(_.getName.endsWith(".csv")).sorted.toSeq
      files.map(file => {
        new Table(file.getName, getFullContentsFromCsvFile(new FileReader(file)), tokenizer)
      }).toIndexedSeq
    }
  }
  logger.debug(s"${allTables.size} tables loaded")

  private val allTableNames = allTables.map(_.fileName)
  logger.debug("tables with internal IDs:\n\t" + allTableNames.zipWithIndex.toString())
  if (internalLogger.isTraceEnabled) allTables.foreach(t => logger.trace(t.titleRow.mkString(",")))

  /** a sequence of table indices to ignore */
  logger.info("Ignoring table IDs " + ignoreList.toString())

  if (params.useCachedTablesForQuestion) {
    logger.info(s"Using CACHED tables for questions from ${params.questionToTablesCache}")
  } else {
    logger.info("Using RANKED tables for questions")
  }

  /** pairs of columns (in two tables) that are allowed to be aligned */
  val allowedColumnAlignments: Seq[AllowedColumnAlignment] = readAllowedColumnAlignments()

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
      getFullContentsFromCsvFile(Utils.getResourceAsReader(params.questionToTablesCache)),
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
          s"\ttable $id (score $score, selected ${rowIds.size} rows}) : " +
            allTables(id).titleRow.mkString("|")
        }
      }.mkString("\n"))
    tableSelections
  }

  /** Get a subset of tables relevant for a given question, by looking up a cheat sheet. */
  private def getCachedTablesForQuestion(question: String): IndexedSeq[TableSelection] = {
    val tableIds: Seq[Int] = questionToTablesMap.getOrElse(question.trim, Seq.empty)
    // TODO: consider having cached table matching scores or using the tfidfTableScore() heuristic.
    // Currently using a default score of 1 and the first K rows.
    val tableSelections = tableIds.map { id =>
      val score = 1d
      val rowIds = allTables(id).contentMatrix.indices.take(params.maxRowsPerTable)
      TableSelection(id, score, rowIds)
    }
    tableSelections.toIndexedSeq
  }

  /** Get a selection of tables relevant for a given question, using quick TF-IDF computation. */
  private def getRankedTablesForQuestion(question: String): IndexedSeq[TableSelection] = {
    // score all tables using tf-idf
    val scoreTables = allTables.indices.diff(ignoreList).map { tableIdx =>
      (tableIdx, tfidfTableScore(tokenizer, tableIdx, question))
    }
    // identify a few top scoring tables
    val topScoredTables = if (!params.useRankThreshold) {
      scoreTables.sortBy(-_._2).slice(0, params.maxTablesPerQuestion)
    } else {
      scoreTables.filter(_._2 > params.rankThreshold)
    }
    // identify most promising rows within each table, turn into a TableSelection
    val questionTokens = tokenizer.stemmedKeywordTokenize(question.toLowerCase)
    topScoredTables.map {
      case (tableIdx, score) => {
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
        val topRowIds = rowIdsWithScores.sortBy(-_._2).take(params.maxRowsPerTable).map(_._1)
        TableSelection(tableIdx, score, topRowIds)
      }
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

  /* read allowed column alignments (across pairs of tables) from a file */
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
      val csvReader = new CSVReader(Utils.getResourceAsReader(alignmentsFile))
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

  /* read relations between pairs of columns in a table */
  private def readAllowedRelations(): Seq[InterColumnRelation] = {
    val file = if (params.useTablestoreFormat) {
      params.columnRelationsTablestoreFile
    } else {
      params.columnRelationsFile
    }
    if (file.isEmpty) {
      Seq.empty
    } else {
      val csvReader = new CSVReader(Utils.getResourceAsReader(file))
      val fullContents: Seq[Seq[String]] = csvReader.readAll.asScala.map(_.toSeq)
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

  /* read how various relations may be represented lexically */
  private def readRelationRepresentations(file: String): Map[String, Seq[RelationPattern]] = {
    if (file.isEmpty) {
      Map.empty
    } else {
      val csvReader = new CSVReader(Utils.getResourceAsReader(file))
      val fullContents: Seq[Seq[String]] = csvReader.readAll.asScala.map(_.toSeq)
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
}

package org.allenai.ari.solvers.tableilp.params

import com.google.inject.Inject
import com.google.inject.name.Named
import com.typesafe.config.{ ConfigFactory, Config }

/** Various parameters related to knowledge tables.
  *
  * @param useLocal whether to read tables from a local folder or from Datastore
  * @param localFolder name of local folder from which to read tables, if useLocal = true and
  *     useTablestore = false
  * @param datastoreFolderConfig Datastore folder from which to read tables, if useLocal = false
  *     and useTablestore = false
  * @param useTablestore use Tablestore data in json format instead of csv files
  * @param localTablestoreFile location of file from which to read tables, if useLocal = true and
  *     useTablestore = true
  * @param datastoreTablestoreConfig Datastore location of file from which to read tables, if
  *     useLocal = false and useTablestore = true
  * @param ignoreListStr a comma-separated list of table IDs to ignore
  * @param maxTablesPerQuestion max number of tables to consider per question
  * @param questionToTablesCache name of a debugging cheat sheet mapping question to relevant tables
  * @param useCachedTablesForQuestion whether to use the above cheat sheet
  * @param rankThreshold table rank must be at most this much for selection, if useThreshold = true
  * @param useRankThreshold whether to use the above threshold on rank
  * @param allowedColumnAlignmentsFile a CSV file specifying which columns in two tables may align
  */
class TableParams @Inject() (
    @Named("tables.useLocal") val useLocal: Boolean,
    @Named("tables.localFolder") val localFolder: String,
    @Named("tables.datastoreFolder") val datastoreFolderConfig: Config,
    @Named("tables.useTablestore") val useTablestore: Boolean,
    @Named("tables.localTablestoreFile") val localTablestoreFile: String,
    @Named("tables.datastoreTablestoreFile") val datastoreTablestoreConfig: Config,
    @Named("tables.ignoreList") ignoreListStr: String,
    @Named("tables.maxTablesPerQuestion") val maxTablesPerQuestion: Int,
    @Named("tables.questionToTablesCache") val questionToTablesCache: String,
    @Named("tables.useCachedTablesForQuestion") val useCachedTablesForQuestion: Boolean,
    @Named("tables.rankThreshold") val rankThreshold: Double,
    @Named("tables.useRankThreshold") val useRankThreshold: Boolean,
    @Named("tables.allowedColumnAlignmentsFile") val allowedColumnAlignmentsFile: String
) {
  val commaSep = ",".r
  val ignoreList: Seq[Int] = if (ignoreListStr == "") {
    Seq.empty
  } else {
    commaSep.split(ignoreListStr).map(_.toInt)
  }
}

/** An object to capture the default knowledge table parameters */
object TableParams {
  val Default = new TableParams(
    useLocal = true,
    localFolder = "data/SampleTables",
    datastoreFolderConfig = ConfigFactory.empty(),
    useTablestore = false,
    localTablestoreFile = "",
    datastoreTablestoreConfig = ConfigFactory.empty(),
    ignoreListStr = "15",
    maxTablesPerQuestion = 4,
    questionToTablesCache = "",
    useCachedTablesForQuestion = false,
    rankThreshold = 0.25d,
    useRankThreshold = false,
    allowedColumnAlignmentsFile = ""
  )
}

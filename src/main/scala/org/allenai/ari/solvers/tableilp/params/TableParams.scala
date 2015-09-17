package org.allenai.ari.solvers.tableilp.params

import com.google.inject.Inject
import com.google.inject.name.Named

/** Various parameters related to knowledge tables.
  *
  * @param folder Name of the folder from which to read tables
  * @param ignoreListStr A comma-separated list of table IDs to ignore
  * @param maxTablesPerQuestion Max number of tables to consider per question
  * @param questionToTablesCache Name of a debugging cheat sheet mapping question to relevant tables
  * @param useCachedTablesForQuestion Whether to use the above cheat sheet
  * @param rankThreshold Table rank must be at most this much for selection, if useThreshold = true
  * @param useRankThreshold Whether to use the above threshold on rank
  * @param allowedTitleAlignmentsFile A CSV file specifying which columns in two tables may align
  */
class TableParams @Inject() (
    @Named("tables.folder") val folder: String,
    @Named("tables.ignoreList") ignoreListStr: String,
    @Named("tables.maxTablesPerQuestion") val maxTablesPerQuestion: Int,
    @Named("tables.questionToTablesCache") val questionToTablesCache: String,
    @Named("tables.useCachedTablesForQuestion") val useCachedTablesForQuestion: Boolean,
    @Named("tables.rankThreshold") val rankThreshold: Double,
    @Named("tables.useRankThreshold") val useRankThreshold: Boolean,
    @Named("tables.allowedTitleAlignmentsFile") val allowedTitleAlignmentsFile: String
) {
  val ignoreList: Seq[Int] = if (ignoreListStr == "") {
    Seq.empty
  } else {
    ignoreListStr.split(',').map(_.toInt)
  }
}

/** An object to capture the default knowledge table parameters */
object TableParams {
  val Default = new TableParams(
    folder = "data/allTables",
    ignoreListStr = "15",
    maxTablesPerQuestion = 4,
    questionToTablesCache = "",
    useCachedTablesForQuestion = false,
    rankThreshold = 0.25d,
    useRankThreshold = false,
    allowedTitleAlignmentsFile = ""
  )
}

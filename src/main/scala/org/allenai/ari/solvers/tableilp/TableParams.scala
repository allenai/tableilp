package org.allenai.ari.solvers.tableilp

import com.google.inject.Inject
import com.google.inject.name.Named

/** Various parameters related to knowledge tables.
  *
  * @param folder Name of the folder from which to read tables
  * @param ignoreListStr A comma-separated list of able IDs to ignore
  * @param maxTablesPerQuestion Max number of tables to consider per question
  * @param questionToTablesCache Name of the cheat sheet mapping question to relevant tables
  * @param useCachedTablesForQuestion Whether to use the above cheat sheet
  */
class TableParams @Inject() (
    @Named("tables.folder") val folder: String = "data/allTables",
    @Named("tables.ignoreList") ignoreListStr: String = "",
    @Named("tables.maxTablesPerQuestion") val maxTablesPerQuestion: Int = 4,
    @Named("tables.questionToTablesCache") val questionToTablesCache: String = "",
    @Named("tables.useCachedTablesForQuestion") val useCachedTablesForQuestion: Boolean = false
) {
  val ignoreList: Seq[Int] = if (ignoreListStr == "") {
    Seq.empty
  } else {
    ignoreListStr.split(',').map(_.toInt).toSeq
  }
}

/** An object to capture the default knowledge table parameters */
object TableParams {
  val Default = new TableParams()
}

package org.allenai.ari.solvers.tableilp

import org.allenai.ari.solvers.common.KeywordTokenizer
import org.allenai.common.Logging

import au.com.bytecode.opencsv.CSVReader

import java.io.FileReader

import scala.collection.JavaConverters._

case class TokenizedCell(values: Seq[String])

class Table(fileName: String, tokenizer: KeywordTokenizer) extends Logging {
  // config: ignore "gray" columns in the KB tables that act as textual fillers between columns
  val ignoreTableColumnFillers = true
  // config: ignore columns whose title starts with the word SKIP
  val ignoreTableColumnsMarkedSkip = true
  // config: keep tokenized values
  val tokenizeCells = true

  // title row, key columns, the rest of the content matrix, and also tokenized content cells if
  // tokenizeCells = true
  val (titleRow, keyColumns, contentMatrix, fullContentNormalized) = readCSV(fileName)

  /** Create a Table by reading a CSV file with the following convention:
    * //   columns with header starting with prefix "KEY" are designated as key columns;
    * //   columns with header starting with the prefix "SKIP" are skipped
    *
    * @param file a CSV file with a header
    * @return a tuple (titleRow, keyColumns, contentMatrix, fullContentMatrixNormalized)
    */
  private def readCSV(
    file: String
  ): (Seq[String], Seq[Int], Seq[Seq[String]], Seq[Seq[TokenizedCell]]) = {
    val reader = new CSVReader(new FileReader(file))
    val fullContents: Seq[Seq[String]] = reader.readAll.asScala.map(_.toSeq)

    val sep = "\\s+".r
    val filteredColIndices = for {
      (title, idx) <- fullContents.head.zipWithIndex
      if !ignoreTableColumnFillers || title != ""
      // skip columns whose header starts with the word "SKIP"
      if !ignoreTableColumnsMarkedSkip || sep.split(title)(0) != "SKIP"
    } yield idx
    val fullContentsFiltered = fullContents.map(filteredColIndices collect _)

    // optionally tokenize every cell (include column headers) of the table
    // TODO: create a case class for each cell to keep its rawText, tokens, etc., together
    val fullContentTokenized = if (tokenizeCells) {
      fullContentsFiltered.map { row =>
        row.map(cell => TokenizedCell(tokenizer.stemmedKeywordTokenize(cell)))
      }
    } else {
      Seq(Seq[TokenizedCell]())
    }

    val titleRowOrig = fullContentsFiltered.head
    // retrieve indices of columns whose header starts with the word "KEY"
    val keyColumns = titleRowOrig.zipWithIndex.filter(_._1.startsWith("KEY ")).map(_._2)
    val titleRow = titleRowOrig.map(_.stripPrefix("KEY "))
    val contentMatrix = fullContentsFiltered.tail
    (titleRow, keyColumns, contentMatrix, fullContentTokenized)
  }
}

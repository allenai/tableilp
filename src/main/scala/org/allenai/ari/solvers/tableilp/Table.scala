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

  // extract title row and the rest of the content matrix from a CSV file
  val (titleRow, contentMatrix, fullContentNormalized) = readCSV(fileName)

  private def readCSV(file: String): (Seq[String], Seq[Seq[String]], Seq[Seq[TokenizedCell]]) = {
    val reader = new CSVReader(new FileReader(file))
    val fullContents: Seq[Seq[String]] = reader.readAll.asScala.map(_.toSeq)

    val sep = "\\s+".r
    val filteredColIndices = for {
      (title, idx) <- fullContents.head.zipWithIndex
      if !ignoreTableColumnFillers || title != ""
      if !ignoreTableColumnsMarkedSkip || sep.split(title)(0) != "SKIP"
    } yield idx
    val fullContentsFiltered = fullContents.map(filteredColIndices collect _)

    val fullContentNormalized = if (tokenizeCells) {
      fullContentsFiltered.map(row => row.map(cell => TokenizedCell(tokenizer.stemmedKeywordTokenize(cell))))
    } else {
      Seq(Seq[TokenizedCell]())
    }

    (fullContentsFiltered.head, fullContentsFiltered.tail, fullContentNormalized)
  }
}

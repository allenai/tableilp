package org.allenai.ari.solvers.tableilp

import org.allenai.common.Logging

import au.com.bytecode.opencsv.CSVReader

import java.io.FileReader

import scala.collection.JavaConverters._

class Table(fileName: String) extends Logging {
  // config: ignore "gray" columns in the KB tables that act as textual fillers between columns
  val ignoreTableColumnFillers = true

  // extract title row and the rest of the content matrix from a CSV file
  val (titleRow, contentMatrix) = readCSV(fileName)

  private def readCSV(file: String): (Seq[String], Seq[Seq[String]]) = {
    val reader = new CSVReader(new FileReader(file))
    val fullContents: Seq[Seq[String]] = reader.readAll.asScala.map(_.toSeq)

    val fullContentsFiltered = {
      if (ignoreTableColumnFillers) {
        val indices = fullContents.head.zipWithIndex.filter(_._1 != "").map(_._2)
        fullContents.map(indices collect _)
      } else {
        fullContents
      }
    }
    (fullContentsFiltered.head, fullContentsFiltered.tail)
  }
}

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

object TableInterface extends Logging {
  def loadAllTables(): Seq[Table] = {
    val files = new java.io.File("src/main/resources/allTables")
      .listFiles.filter(_.getName.endsWith(".csv"))
    logger.debug("Here are the table names:\n" + files.mkString("\n"))
    val tables = files.map((file) => new Table(file.getAbsolutePath))
    if (internalLogger.isDebugEnabled) tables.foreach(t => logger.debug(t.titleRow.mkString(",")))
    val tablesShortened = tables.slice(0, 4)
    tablesShortened
  }

  def loadTables(): Seq[Table] = {
    val path = "src/main/resources/tables/"
    val files = Seq("SampleTable-Country-Hemisphere.csv", "SampleTable-Season-Month.csv")
    val tables = files.map((file) => new Table(path + file))
    if (internalLogger.isDebugEnabled) tables.foreach(t => logger.debug(t.titleRow.mkString(",")))
    tables
  }

  def loadTableForQuestion(question: String): Seq[Table] = {
    val allTables = loadAllTables()
    val informationTable = new Table("src/main/resources/table-question-information.csv")
    val questionToTablesOpt = informationTable.contentMatrix.find(_(1) == question) match {
      case qToTablesOpt => qToTablesOpt
      case None => informationTable.contentMatrix.find(_(1).trim == question.trim)
    }
    val tables = questionToTablesOpt match {
      case Some(qToTables) => qToTables(2).split('-').map(idx => allTables(idx.toInt)).toSeq
      case None => Seq.empty
    }
    tables
  }

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
}

package org.allenai.ari.solvers.tableilp

import org.allenai.common.Logging

import au.com.bytecode.opencsv.CSVReader

import java.io.FileReader

import scala.collection.JavaConverters._

class Table(fileName: String) extends Logging {
  val (titleRow, contentMatrix) = readCSV(fileName)

  // reading from csv: for future
  def readCSV(file: String): (Array[String], Seq[Array[String]]) = {
    val reader = new CSVReader(new FileReader(file))
    val fullContents: Seq[Array[String]] = reader.readAll.asScala

    val REMOVE_FILLERS = true // TODO: move to the config file?
    val fullContentsFiltered = {
      if (REMOVE_FILLERS) {
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
  def loadAllTables(): Array[Table] = {
    val files = new java.io.File("src/main/resources/allTables")
      .listFiles.filter(_.getName.endsWith(".csv"))
    logger.debug("Here are the table names:\n" + files.mkString("\n"))
    val tables = files.map((file) => new Table(file.getAbsolutePath))
    if (internalLogger.isDebugEnabled) tables.foreach(t => logger.debug(t.titleRow.mkString(",")))
    val tablesShortened = tables.slice(0, 4)
    tablesShortened
  }

  def loadTables(): Array[Table] = {
    val path = "src/main/resources/tables/"
    val files = Seq("SampleTable-Country-Hemisphere.csv", "SampleTable-Season-Month.csv")
    val tables = files.map((file) => new Table(path + file))
    if (internalLogger.isDebugEnabled) tables.foreach(t => logger.debug(t.titleRow.mkString(",")))
    tables.toArray
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

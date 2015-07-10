package org.allenai.ari.solvers.tableilp

import org.allenai.ari.solvers.tableilp.ilpsolver.ScipInterface
import org.allenai.common.Logging

import spray.json.DefaultJsonProtocol._
import spray.json._

import scala.collection.mutable.ArrayBuffer

/** All table cells and question constituent that a string aligns with in the solution. */
case class StringAlignmentPair(
  string: String,
  alignment: ArrayBuffer[Int]
)
/** This variable keeps the contents of a table, and how its cells are aligned with other
  * information in the problem, including other tables, the question definition and its choices.
  * Later we feed this into the visualizer. Talk to Daniel, if you want to change this definition!
  */
case class TableAlignment(
  titleRow: Array[StringAlignmentPair],
  contentMatrix: Array[Array[StringAlignmentPair]]
)
/** This variable keeps the contents of the question definiition and its choices, and how they are
  * aligned with other tables. Later we feed this into the visualizer. So talk to Daniel, if you
  * want to change this definition!
  */
case class QuestionAlignment(
  questionCons: Array[StringAlignmentPair],
  choices: Array[StringAlignmentPair]
)
/** A complete set of alignments between multiple tables and between tables and question
  * constituents. This is the output of the ILP model.
  */
case class AlignmentSolution(
  tables: Array[TableAlignment],
  question: QuestionAlignment,
  bestChoice: Int,
  bestChoiceScore: Double
)

object AlignmentSolution extends Logging {
  // JsonFormat doesn't seem to be available for ArrayBuffer; implement here
  implicit val stringAlignmentJsonFormat = new JsonFormat[StringAlignmentPair] {
    override def read(json: JsValue): StringAlignmentPair = {
      // TODO: implement this!
      new StringAlignmentPair("", ArrayBuffer.empty)
    }
    override def write(stringAlignmentPair: StringAlignmentPair): JsValue = {
      (stringAlignmentPair.string, stringAlignmentPair.alignment.toSeq).toJson
    }
  }
  implicit val tableJsonFormat = jsonFormat2(TableAlignment.apply)
  implicit val questionJsonFormat = jsonFormat2(QuestionAlignment.apply)
  implicit val alignmentSolutionJsonFormat = jsonFormat4(AlignmentSolution.apply)

  private val alignmentThreshold = 0.999

  def generateAlignmentSolution(allVariables: AllVariables, scipSolver: ScipInterface,
    question: Question, tables: Array[Table]): AlignmentSolution = {
    val questionChunkAlignmentPair = question.questionCons
      .map(StringAlignmentPair(_, ArrayBuffer.empty))
    val choiceAlignmentPair = question.choices.map(StringAlignmentPair(_, ArrayBuffer.empty))
    val tableAlignments = tables.map { table =>
      val alignedTable = table.contentMatrix.map(_.map(StringAlignmentPair(_, ArrayBuffer.empty)))
      val alignedTitle = table.titleRow.map(StringAlignmentPair(_, ArrayBuffer.empty))
      TableAlignment(alignedTitle, alignedTable.toArray)
    }

    // add the alignment information

    // inter-table alignments
    val interTableAlignmentPairs = for {
      entry <- allVariables.interTableVariables
      if scipSolver.getSolVal(entry.variable) > alignmentThreshold
    } yield {
      val cell1 = tableAlignments(entry.tableIdx1).contentMatrix(entry.rowIdx1)(entry.colIdx1)
      val cell2 = tableAlignments(entry.tableIdx2).contentMatrix(entry.rowIdx2)(entry.colIdx2)
      (cell1, cell2)
    }

    // intra-table alignments
    val intraTableAlignmentPairs = for {
      entry <- allVariables.intraTableVariables
      if scipSolver.getSolVal(entry.variable) > alignmentThreshold
    } yield {
      val cell1 = tableAlignments(entry.tableIdx).contentMatrix(entry.rowIdx)(entry.colIdx1)
      val cell2 = tableAlignments(entry.tableIdx).contentMatrix(entry.rowIdx)(entry.colIdx2)
      (cell1, cell2)
    }

    // question table alignments
    val questionTableAlignmentPairs = for {
      entry <- allVariables.questionTableVariables
      if scipSolver.getSolVal(entry.variable) > alignmentThreshold
    } yield {
      val cell = tableAlignments(entry.tableIdx).contentMatrix(entry.rowIdx)(entry.colIdx)
      val qCons = questionChunkAlignmentPair(entry.qConsIdx)
      (cell, qCons)
    }

    // question title alignments
    val questionTitleAlignmentPairs = for {
      entry <- allVariables.questionTitleVariables
      if scipSolver.getSolVal(entry.variable) > alignmentThreshold
    } yield {
      val cell = tableAlignments(entry.tableIdx).titleRow(entry.colIdx)
      val qCons = questionChunkAlignmentPair(entry.qConsIdx)
      (cell, qCons)
    }

    // choice table alignments
    val choiceTableAlignmentPairs = for {
      entry <- allVariables.qChoiceTableVariables
      if scipSolver.getSolVal(entry.variable) > alignmentThreshold
    } yield {
      val cell = tableAlignments(entry.tableIdx).contentMatrix(entry.rowIdx)(entry.colIdx)
      val qOptCons = choiceAlignmentPair(entry.qConsIdx)
      (cell, qOptCons)
    }

    // populate `alignment' fields of alignment pairs with a unique alignmentId
    val allAlignmentPairs = interTableAlignmentPairs ++ intraTableAlignmentPairs ++
      questionTableAlignmentPairs ++ questionTitleAlignmentPairs ++ choiceTableAlignmentPairs
    val cellToAlignmentId = allAlignmentPairs.zipWithIndex.flatMap {
      case ((strAlign1, strAlign2), alignmentId) =>
        Seq((strAlign1, alignmentId), (strAlign2, alignmentId))
    }
    // TODO: is it possible to do with without cell.alignment being an ArrayBuffer?
    // Note that it is assigned a value exactly once, with the "++=" below.
    // One way could be to keep separate matrices for strings and for alignment IDs
    // rather than a joint pair, StringAlignmentPair.
    cellToAlignmentId.groupBy(c => System.identityHashCode(c._1)).foreach {
      case (_, cellWithAlignmentIds) =>
        cellWithAlignmentIds.head._1.alignment ++= cellWithAlignmentIds.map(_._2)
    }

    // create a new question alignment object
    val questionAlignmentPair = new QuestionAlignment(
      questionChunkAlignmentPair,
      choiceAlignmentPair
    )

    // choose answer choice and its score
    logger.debug("the number of the choices = " + allVariables.qChoiceTableVariables.length)
    val (bestChoiceIdx, bestChoiceScore) = if (allVariables.qChoiceTableVariables.nonEmpty) {
      val idx = allVariables.qChoiceTableVariables.map { choice =>
        scipSolver.getSolVal(choice.variable)
      }.zipWithIndex.maxBy(_._1)._2
      //      val score = scipSolver.getSolVal(
      //        allVariables.qChoiceTableVariables(idx).variable
      //      )
      val choiceIdx = allVariables.qChoiceTableVariables(idx).qConsIdx
      (choiceIdx, scipSolver.getPrimalbound)
    } else {
      (1, 1.0)
    }

    // return an alignment solution
    AlignmentSolution(tableAlignments, questionAlignmentPair, bestChoiceIdx, bestChoiceScore)
  }

  val generateSampleAlignmentSolution: AlignmentSolution = {
    val r = scala.util.Random
    val questionChunks = Array("In", "New York State", "the", "shortest", "period",
      "of", "daylight", "occurs", "during", "which", "month")
    val questionChunkAlignmentPair = questionChunks.map { chunk =>
      val randSize = r.nextInt(4)
      val randomAlignment = (0 until randSize).map(_ => r.nextInt(13))
      StringAlignmentPair(chunk, randomAlignment.to[ArrayBuffer])
    }
    val choices = Array("January", "December", "June", "July")
    val choicesAlignmentPair = choices.map { chunk =>
      val randSize = r.nextInt(4)
      val randomAlignment = (0 until randSize).map(_ => r.nextInt(13))
      StringAlignmentPair(chunk, randomAlignment.to[ArrayBuffer])
    }
    val questionAlignmentPair = new QuestionAlignment(
      questionChunkAlignmentPair,
      choicesAlignmentPair
    )

    val tables = TableInterface.loadTables()
    val tablesAlignment = tables.map { table =>
      val alignedTable = table.contentMatrix.map { row =>
        row.map { cell =>
          val randSize = r.nextInt(4)
          val randomAlignment = (0 until randSize).map(_ => r.nextInt(13))
          StringAlignmentPair(cell, randomAlignment.to[ArrayBuffer])
        }
      }
      val alignedTitle = table.titleRow.map { cell =>
        val randSize = r.nextInt(4)
        val randomAlignment = (0 until randSize).map(_ => r.nextInt(13))
        StringAlignmentPair(cell, randomAlignment.to[ArrayBuffer])
      }
      TableAlignment(alignedTitle, alignedTable.toArray)
    }
    AlignmentSolution(tablesAlignment, questionAlignmentPair, 1, 0.5)
  }

  def main(args: Array[String]) {
    val alignment = generateSampleAlignmentSolution
    println(alignment.toJson)
  }
}

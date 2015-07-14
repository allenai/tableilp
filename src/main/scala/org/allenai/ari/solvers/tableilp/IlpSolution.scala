package org.allenai.ari.solvers.tableilp

import org.allenai.ari.solvers.tableilp.ilpsolver.ScipInterface
import org.allenai.common.Logging

import spray.json.DefaultJsonProtocol._
import spray.json._

import scala.collection.mutable.ArrayBuffer

/** The alignment of a basic alignment unit in the ILP solution.
  *
  * @param unit A basic alignment unit (a word, a chunk, a string associated with a cell, etc)
  * @param alignments A sequence of alignments of the above unit
  */
case class UnitAlignment(
    unit: String,
    alignments: ArrayBuffer[Int]
) {
  def this(str: String) = this(str, ArrayBuffer.empty)
}

/** All alignments of each cell in a table's title row and content matrix to everything else.
  *
  * @param titleAlignments A UnitAlignment for each cell in the title row
  * @param contentAlignments A UnitAlignment for each cell in the content matrix
  */
case class TableAlignment(
  titleAlignments: Seq[UnitAlignment],
  contentAlignments: Seq[Seq[UnitAlignment]]
)

/** All alignments of each question constituent and each choice to everything else.
  *
  * @param qConsAlignments A UnitAlignment for each question constituent
  * @param choiceAlignments A UnitAlignment for each answer choice
  */
case class QuestionAlignment(
  qConsAlignments: Seq[UnitAlignment],
  choiceAlignments: Seq[UnitAlignment]
)

/** A complete set of alignments between multiple tables and between tables and question
  * constituents. This is the output of the ILP model.
  *
  * @param tableAlignments A sequence of alignment information, one per table
  * @param questionAlignment Alignments for the question (including constituents and choices)
  * @param bestChoice The best choice determined by the ILP model
  * @param bestChoiceScore The score for the best choice (the ILP objective value)
  */
case class IlpSolution(
  tableAlignments: Seq[TableAlignment],
  questionAlignment: QuestionAlignment,
  bestChoice: Int,
  bestChoiceScore: Double
)

/** A container object to define Json protocol and have a main testing routine */
object IlpSolution {
  // The default json protocol has JsonFormat implemented only for immutable collections. Add it
  // for ArrayBuffer here.
  // Note: lift turns a one-sided converter (Writer or Reader) into a symmetric format that throws
  // an exception if an unimplemented method is called.
  implicit val unitAlignmentJsonFormat: JsonFormat[UnitAlignment] = lift(
    { pair: UnitAlignment => (pair.unit, pair.alignments.toSeq).toJson }
  )
  implicit val tableAlignmentJsonFormat = jsonFormat2(TableAlignment.apply)
  implicit val questionAlignmentJsonFormat = jsonFormat2(QuestionAlignment.apply)
  implicit val ilpSolutionJsonFormat = jsonFormat4(IlpSolution.apply)

  /** Main method to test a sample alignment solution */
  def main(args: Array[String]) {
    val ilpSolution = IlpSolutionFactory.makeRandomIlpSolution
    println(ilpSolution.toJson)
  }
}

/** A container object to generate IlpSolution object based on the ILP model output */
object IlpSolutionFactory extends Logging {
  /** config: a threshold above which alignment is considered active */
  private val alignmentThreshold = 0.999

  /** Process the solution found by SCIP to deduce which parts of the question + tables align with
    * which other parts. This information can then be visualized or presented in another format.
    * @param allVariables all core decision variables in the ILP model
    * @param scipSolver a reference to the SCIP solver object
    * @param question the question
    * @param tables the tables used
    * @return an AlignmentSolution object
    */
  def makeIlpSolution(allVariables: AllVariables, scipSolver: ScipInterface, question: Question,
    tables: Seq[Table]): IlpSolution = {
    val qConsAlignments = question.questionCons.map(new UnitAlignment(_))
    val choiceAlignments = question.choices.map(new UnitAlignment(_))
    val tableAlignments = tables.map { table =>
      val titleAlignments = table.titleRow.map(new UnitAlignment(_))
      val contentAlignments = table.contentMatrix.map(_.map(new UnitAlignment(_)))
      TableAlignment(titleAlignments, contentAlignments)
    }

    // inter-table alignments
    val interTableAlignmentPairs: IndexedSeq[(UnitAlignment, UnitAlignment)] = for {
      entry <- allVariables.interTableVariables
      if scipSolver.getSolVal(entry.variable) > alignmentThreshold
    } yield {
      val cell1 = tableAlignments(entry.tableIdx1).contentAlignments(entry.rowIdx1)(entry.colIdx1)
      val cell2 = tableAlignments(entry.tableIdx2).contentAlignments(entry.rowIdx2)(entry.colIdx2)
      (cell1, cell2)
    }

    // intra-table alignments
    val intraTableAlignmentPairs: IndexedSeq[(UnitAlignment, UnitAlignment)] = for {
      entry <- allVariables.intraTableVariables
      if scipSolver.getSolVal(entry.variable) > alignmentThreshold
    } yield {
      val cell1 = tableAlignments(entry.tableIdx).contentAlignments(entry.rowIdx)(entry.colIdx1)
      val cell2 = tableAlignments(entry.tableIdx).contentAlignments(entry.rowIdx)(entry.colIdx2)
      (cell1, cell2)
    }

    // question table alignments
    val questionTableAlignmentPairs: IndexedSeq[(UnitAlignment, UnitAlignment)] = for {
      entry <- allVariables.questionTableVariables
      if scipSolver.getSolVal(entry.variable) > alignmentThreshold
    } yield {
      val cell = tableAlignments(entry.tableIdx).contentAlignments(entry.rowIdx)(entry.colIdx)
      val qCons = qConsAlignments(entry.qConsIdx)
      (cell, qCons)
    }

    // question title alignments
    val questionTitleAlignmentPairs: IndexedSeq[(UnitAlignment, UnitAlignment)] = for {
      entry <- allVariables.questionTitleVariables
      if scipSolver.getSolVal(entry.variable) > alignmentThreshold
    } yield {
      val cell = tableAlignments(entry.tableIdx).titleAlignments(entry.colIdx)
      val qCons = qConsAlignments(entry.qConsIdx)
      (cell, qCons)
    }

    // choice table alignments
    val choiceTableAlignmentPairs: IndexedSeq[(UnitAlignment, UnitAlignment)] = for {
      entry <- allVariables.qChoiceTableVariables
      if scipSolver.getSolVal(entry.variable) > alignmentThreshold
    } yield {
      val cell = tableAlignments(entry.tableIdx).contentAlignments(entry.rowIdx)(entry.colIdx)
      val qOptCons = choiceAlignments(entry.qConsIdx)
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
        cellWithAlignmentIds.head._1.alignments ++= cellWithAlignmentIds.map(_._2)
    }

    // create a new question alignment object
    val questionAlignment = QuestionAlignment(qConsAlignments, choiceAlignments)

    // choose answer choice and its score
    logger.debug("the number of the choices = " + allVariables.qChoiceTableVariables.length)
    val (bestChoiceIdx, bestChoiceScore) = if (allVariables.qChoiceTableVariables.nonEmpty) {
      val idx = allVariables.qChoiceTableVariables.map { choice =>
        scipSolver.getSolVal(choice.variable)
      }.zipWithIndex.maxBy(_._1)._2
      val choiceIdx = allVariables.qChoiceTableVariables(idx).qConsIdx
      (choiceIdx, scipSolver.getPrimalbound)
    } else {
      // The default, helpful for initial debugging
      (1, 1.0)
    }

    // return the alignment solution
    IlpSolution(tableAlignments, questionAlignment, bestChoiceIdx, bestChoiceScore)
  }

  /** Load all tables, if and when needed */
  private lazy val tables = TableInterface.loadTables()

  /** Generate up to maxNVals random integers, each less than maxVal */
  private val r = scala.util.Random
  private val maxNVals = 4
  private val maxVal = 13
  private def nextRandInts(): Seq[Int] = {
    val nvals = r.nextInt(maxNVals)
    (0 until nvals).map(_ => r.nextInt(maxVal))
  }

  /** A sample question */
  private val questionChunks = Array("In", "New York State", "the", "shortest", "period",
    "of", "daylight", "occurs", "during", "which", "month")
  /** Sample answer choices */
  private val choices = Array("January", "December", "June", "July")

  /** Generate a random alignment solution object for visualizer testing */
  def makeRandomIlpSolution: IlpSolution = {
    val qConsAlignments = questionChunks.map(UnitAlignment(_, nextRandInts().to[ArrayBuffer]))
    val choiceAlignments = choices.map(UnitAlignment(_, nextRandInts().to[ArrayBuffer]))
    val questionAlignment = QuestionAlignment(qConsAlignments, choiceAlignments)
    val tablesAlignments = tables.map { table =>
      val titleAlignments = table.titleRow.map { cell =>
        UnitAlignment(cell, nextRandInts().to[ArrayBuffer])
      }
      val contentAlignments = table.contentMatrix.map { row =>
        row.map(UnitAlignment(_, nextRandInts().to[ArrayBuffer]))
      }
      TableAlignment(titleAlignments, contentAlignments)
    }
    IlpSolution(tablesAlignments, questionAlignment, 1, 0.5)
  }
}

package org.allenai.ari.solvers.tableilp

import org.allenai.ari.solvers.tableilp.ilpsolver.ScipInterface
import org.allenai.common.Logging

/** An index into a cell in a table */
case class CellIdx(tableIdx: Int, rowIdx: Int, colIdx: Int)

/** A title index in a table */
case class TitleIdx(tableIdx: Int, colIdx: Int)

/** All non-auxiliary variables in the ILP model.
  *
  * @param intraTableVariables variables involving two cells in the same table
  * @param interTableVariables variables involving two cells in different tables
  * @param questionTableVariables variables involving a question constituent and a table cell
  * @param questionTitleVariables variables involving a question constituent and a table title cell
  * @param qChoiceTableVariables variables involving an choice in a question constituent and a table
  */
case class AllVariables(
    intraTableVariables: IndexedSeq[IntraTableVariable],
    interTableVariables: IndexedSeq[InterTableVariable],
    questionTableVariables: IndexedSeq[QuestionTableVariable],
    questionTitleVariables: IndexedSeq[QuestionTitleVariable],
    qChoiceTableVariables: IndexedSeq[QuestionTableVariable]
) {
  def ++(that: AllVariables): AllVariables = {
    AllVariables(
      intraTableVariables ++ that.intraTableVariables,
      interTableVariables ++ that.interTableVariables,
      questionTableVariables ++ that.questionTableVariables,
      questionTitleVariables ++ that.questionTitleVariables,
      qChoiceTableVariables ++ that.qChoiceTableVariables
    )
  }

  lazy val getIlpVars: Seq[Long] = {
    intraTableVariables.map(_.variable) ++ interTableVariables.map(_.variable) ++
      questionTableVariables.map(_.variable) ++ questionTitleVariables.map(_.variable) ++
      qChoiceTableVariables.map(_.variable)
  }
}

/** Variables involving two cells within a single row of a table.
  *
  * @param tableIdx index identifying a table
  * @param rowIdx index identifying a row in the table
  * @param colIdx1 index identifying the first column
  * @param colIdx2 index identifying the second column
  * @param variable a pointer to the associated ILP variable
  */
case class IntraTableVariable(
  tableIdx: Int,
  rowIdx: Int,
  colIdx1: Int,
  colIdx2: Int,
  variable: Long
)

/** Variables involving two cells in two different tables.
  *
  * @param tableIdx1 index identifying the first table
  * @param tableIdx2 index identifying the second table
  * @param rowIdx1 index identifying a row in the first table
  * @param rowIdx2 index identifying a row in the second table
  * @param colIdx1 index identifying a column in the first table
  * @param colIdx2 index identifying a column in the second table
  * @param variable a pointer to the associated ILP variable
  */
case class InterTableVariable(
  tableIdx1: Int,
  tableIdx2: Int,
  rowIdx1: Int,
  rowIdx2: Int,
  colIdx1: Int,
  colIdx2: Int,
  variable: Long
)

/** Variables involving a question constituent and a table cell.
  *
  * @param qConsIdx index identifying a constitute in the question
  * @param tableIdx index identifying a table
  * @param rowIdx index identifying a row in the table
  * @param colIdx index identifying a column in the table
  * @param variable a pointer to the associated ILP variable
  */
case class QuestionTableVariable(
  qConsIdx: Int,
  tableIdx: Int,
  rowIdx: Int,
  colIdx: Int,
  variable: Long
)

/** Variables involving a question constituent and a table title cell.
  *
  * @param qConsIdx index identifying a constitute in the question
  * @param tableIdx index identifying a table
  * @param colIdx index identifying a column in the table
  * @param variable a pointer to the associated ILP variable
  */
case class QuestionTitleVariable(
  qConsIdx: Int,
  tableIdx: Int,
  colIdx: Int,
  variable: Long
)

/** The ILP model for Table Inference.
  *
  * @param ilpSolver a ScipInterface object
  * @param tables an array of tables as the knowledge base
  * @param aligner a blackbox to align textual mentions
  */
class IlpModel(
    ilpSolver: ScipInterface, tables: Array[Table], aligner: AlignmentFunction
) extends Logging {

  // config: minimum thresholds for alignment
  private val minCellCellAlignmentThreshold = 0.2
  private val minCellQConstituentAlignmentThreshold = 0.2
  private val minTitleQConstituentAlignmentThreshold = 0.2

  // large positive double value to use in constraints
  // TODO(ashish33) consider using SCIP's built-in infinity value, if available
  private val largeDbl = 10000d

  // these set of words in the question text will be ignored before alignment
  private val ignoredWords = Set("?", ":", ",")

  /** Auxiliary variables: whether a cell within a given table is "active" */
  private val activeCellVars: Map[CellIdx, Long] = (for {
    tableIdx <- tables.indices
    table = tables(tableIdx)
    rowIdx <- table.contentMatrix.indices
    row = table.contentMatrix(rowIdx)
    colIdx <- row.indices
    x = ilpSolver.createBinaryVar(s"activeCell_t=${tableIdx}_r=${rowIdx}_c=$colIdx", 0d)
  } yield {
    ilpSolver.addVar(x)
    CellIdx(tableIdx, rowIdx, colIdx) -> x
  }).toMap

  /** Auxiliary variables: whether a row within a given table is "active" */
  private val activeRowVars: Map[(Int, Int), Long] = (for {
    tableIdx <- tables.indices
    table = tables(tableIdx)
    rowIdx <- table.contentMatrix.indices
    x = ilpSolver.createBinaryVar(s"activeRow_t=${tableIdx}_r=$rowIdx", 0d)
  } yield {
    ilpSolver.addVar(x)
    (tableIdx, rowIdx) -> x
  }).toMap

  /** Auxiliary variables: whether a column within a given table is "active" */
  private val activeColVars: Map[(Int, Int), Long] = (for {
    tableIdx <- tables.indices
    table = tables(tableIdx)
    if table.contentMatrix.indices.nonEmpty
    colIdx <- table.contentMatrix.head.indices
    objCoeff = 1d / table.contentMatrix.head.indices.size // prefer more columns matching
    x = ilpSolver.createBinaryVar(s"activeCol_t=${tableIdx}_r=$colIdx", objCoeff)
  } yield {
    ilpSolver.addVar(x)
    (tableIdx, colIdx) -> x
  }).toMap

  /** Auxiliary variables: whether a title column of a given table is "active" */
  private val activeTitleVars: Map[(Int, Int), Long] = (for {
    tableIdx <- tables.indices
    table = tables(tableIdx)
    colIdx <- table.titleRow.indices
    objCoeff = 0.3 // TODO: tune this!
    x = ilpSolver.createBinaryVar(s"activeTitle_t=${tableIdx}_r=$colIdx", objCoeff)
  } yield {
    ilpSolver.addVar(x)
    (tableIdx, colIdx) -> x
  }).toMap

  /** The main method to build the question independent aspects of the ILP model.
    * Note: using 'val' rather than 'def' makes this be computed only once.
    *
    * @return an array of (a subset of) variables of interest whose values may be queried later
    */
  private val buildQuestionIndependentModel: AllVariables = {
    /** Intra-table variables */
    val intraTableVariables = for {
      tableIdx <- tables.indices
      table = tables(tableIdx)
      rowIdx <- table.contentMatrix.indices
      row = table.contentMatrix(rowIdx)
      colIdx1 <- row.indices
      colIdx2 <- colIdx1 + 1 until row.length
      x <- addIntraTableVariable(tableIdx, rowIdx, colIdx1, colIdx2)
    } yield x

    /** Inter-table variables */
    val interTableVariables = for {
      tableIdx1 <- tables.indices
      tableIdx2 <- tableIdx1 + 1 until tables.length
      table1 = tables(tableIdx1)
      table2 = tables(tableIdx2)
      rowIdx1 <- table1.contentMatrix.indices
      rowIdx2 <- table2.contentMatrix.indices
      row1 = table1.contentMatrix(rowIdx1)
      row2 = table2.contentMatrix(rowIdx2)
      colIdx1 <- row1.indices
      colIdx2 <- row2.indices
      x <- addInterTableVariable(tableIdx1, tableIdx2, rowIdx1, rowIdx2, colIdx1, colIdx2)
    } yield x

    // allow inter-table cell alignment only if the titles match
    interTableVariables.foreach { entry =>
      val table1Entry = tables(entry.tableIdx1).titleRow(entry.colIdx1)
      val table2Entry = tables(entry.tableIdx2).titleRow(entry.colIdx2)
      if (aligner.title_title(table1Entry, table2Entry) < 0.5) {
        // no good alignment between the titles; disallow inter table alignment
        ilpSolver.chgVarUb(entry.variable, 0d)
      }
    }

    // row activity constraint: allow at most 1 row per table to be active
    // Note: question dependent variables may also make the row active; this constraint will
    // take that into account as well
    tables.indices.foreach { tableIdx =>
      val table = tables(tableIdx)
      val tableRowVars = table.contentMatrix.indices.map(r => activeRowVars((tableIdx, r)))
      ilpSolver.addConsBasicSetpack(s"atMostOneRow_T=$tableIdx", tableRowVars)
    }

    // return all variables
    AllVariables(intraTableVariables, interTableVariables,
      IndexedSeq.empty, IndexedSeq.empty, IndexedSeq.empty)
  }

  /** The main method to build the question dependent aspects of the ILP model.
    *
    * @param question a question
    * @param existingAllVars all variables currently in existence
    * @return an array of (a subset of) variables of interest whose values may be queried later;
    * this will be a superset of existingAllVars
    */
  private def buildQuestionDependentModel(
    question: Question, existingAllVars: AllVariables
  ): AllVariables = {
    val intraTableVariables = existingAllVars.intraTableVariables
    val interTableVariables = existingAllVars.interTableVariables
    val questionTableVariables = for {
      tableIdx <- tables.indices
      qConsIdx <- question.questionCons.indices
      qCons = question.questionCons(qConsIdx)
      if !ignoredWords.contains(qCons)
      table = tables(tableIdx)
      rowIdx <- table.contentMatrix.indices
      row = tables(tableIdx).contentMatrix(rowIdx)
      colIdx <- row.indices
      x <- addQuestionTableVariable(qCons, qConsIdx, tableIdx, rowIdx, colIdx)
    } yield x
    val questionTitleVariables = for {
      tableIdx <- tables.indices
      qConsIdx <- question.questionCons.indices
      qCons = question.questionCons(qConsIdx)
      if !ignoredWords.contains(qCons)
      table = tables(tableIdx)
      colIdx <- table.titleRow.indices
      x <- addQuestionTitleVariable(qCons, qConsIdx, tableIdx, colIdx)
    } yield x
    val qChoiceTableVariables = for {
      tableIdx <- tables.indices
      qChoiceConsIdx <- question.choices.indices
      qChoiceCons = question.choices(qChoiceConsIdx)
      table = tables(tableIdx)
      rowIdx <- table.contentMatrix.indices
      row = tables(tableIdx).contentMatrix(rowIdx)
      colIdx <- row.indices
      x <- addQChoiceTableVariable(qChoiceCons, qChoiceConsIdx, tableIdx, rowIdx, colIdx)
    } yield x

    /** Auxiliary variables: whether a title column of a given table is "active" */
    val ativeChoiceVars: Map[Int, Long] = (for {
      choiceIdx <- question.choices.indices
      objCoeff = 1 // doesn't matter what this is
      x = ilpSolver.createBinaryVar(s"choice=$choiceIdx", objCoeff)
    } yield {
      ilpSolver.addVar(x)
      choiceIdx -> x
    }).toMap

    /** A convenient map from a cell to intra-table ILP variables associated with it */
    val tmpIntraTriples = intraTableVariables.flatMap {
      case IntraTableVariable(tableIdx, rowIdx, colIdx1, colIdx2, x) =>
        val cellIdx1 = CellIdx(tableIdx, rowIdx, colIdx1)
        val cellIdx2 = CellIdx(tableIdx, rowIdx, colIdx2)
        Seq(cellIdx1 -> x, cellIdx2 -> x)
    }
    val cellToIntraTableVars = Utils.toMapUsingGroupByFirst(tmpIntraTriples)

    /** A convenient map from a cell to inter-table ILP variables associated with it */
    val tmpInterTriples = interTableVariables.flatMap {
      case InterTableVariable(tableIdx1, tableIdx2, rowIdx1, rowIdx2, colIdx1, colIdx2, x) =>
        val cellIdx1 = CellIdx(tableIdx1, rowIdx1, colIdx1)
        val cellIdx2 = CellIdx(tableIdx2, rowIdx2, colIdx2)
        Seq(cellIdx1 -> x, cellIdx2 -> x)
    }
    val cellToInterTableVars = Utils.toMapUsingGroupByFirst(tmpInterTriples)

    /** A convenient map from a cell to question-table ILP variables associated with it */
    val tmpQuestionTriples = questionTableVariables.map {
      case QuestionTableVariable(_, tableIdx, rowIdx, colIdx, x) =>
        CellIdx(tableIdx, rowIdx, colIdx) -> x
    }
    val cellToQuestionTableVars = Utils.toMapUsingGroupByFirst(tmpQuestionTriples)

    val tmpChoicesTriples = qChoiceTableVariables.map {
      case QuestionTableVariable(_, tableIdx, rowIdx, colIdx, x) =>
        CellIdx(tableIdx, rowIdx, colIdx) -> x
    }

    val tmpChoices = qChoiceTableVariables.map {
      case QuestionTableVariable(qChoiceCons, _, _, _, x) =>
        qChoiceCons -> x
    }

    val choiceToExtAlignmentVars = Utils.toMapUsingGroupByFirst(tmpChoices)

    /** Collect all external alignment variables per cell; note: simply doing
      * cellToInterTableVars ++ cellToInterTableVars may not work
      */
    val cellToExtAlignmentVars = Utils.toMapUsingGroupByFirst(
      tmpInterTriples ++ tmpQuestionTriples ++ tmpChoicesTriples
    )

    val tmpTitleToQuestionVars = questionTitleVariables.map {
      case QuestionTitleVariable(_, tableIdx, colIdx, x) =>
        TitleIdx(tableIdx, colIdx) -> x
    }

    /** Collect all external alignments per title */
    val titleToExtAlignmentVars = Utils.toMapUsingGroupByFirst(tmpTitleToQuestionVars)

    // add question dependent activity constraints
    tables.indices.foreach { tableIdx =>
      val table = tables(tableIdx)
      table.contentMatrix.indices.foreach { rowIdx =>
        val activeRowVar = activeRowVars((tableIdx, rowIdx))
        val row = table.contentMatrix(rowIdx)
        val activeCellVarsInRow = row.indices.map { colIdx =>
          val cellIdx = CellIdx(tableIdx, rowIdx, colIdx)
          val activeCellVar = activeCellVars(cellIdx)
          val activeColVar = activeColVars((tableIdx, colIdx))
          // if any variable aligning to a cell is 1, make the corresponding activeCellVar be 1
          val extAlignmentVarsForCell = cellToExtAlignmentVars.getOrElse(cellIdx, Seq.empty)
          extAlignmentVarsForCell.foreach { ilpSolver.addConsXLeqY("activeCell", _, activeCellVar) }
          // if an activeCellVar is 1, at least one external cell alignment variable must be 1;
          // model as sum(extAlignmentVarsForCell) >= activeCellVar, i.e.,
          // 0 <= sum(extAlignmentVarsForCell) - activeCellVar
          val vars = extAlignmentVarsForCell :+ activeCellVar
          val coeffs = Seq.fill(extAlignmentVarsForCell.size)(1d) :+ (-1d)
          ilpSolver.addConsBasicLinear("activeCellImpliesAtLeastOne", vars, coeffs, 0d, largeDbl)
          // if any activeCellVar for a row is 1, make the corresponding activeRowVar be 1
          ilpSolver.addConsXLeqY("activeRow", activeCellVar, activeRowVar)
          ilpSolver.addConsXLeqY("activeCol", activeCellVar, activeColVar)
          // return the active cell var
          activeCellVar
        }
        // non-redundant use of tables: if a row is active, at least TWO of its cells must be
        // active; model as sum(activeCellVarsInRow) >= 2*activeRowVar, i.e.,
        // 0 <= sum(activeCellVarsInRow) - 2*activeRowVar
        val vars = activeCellVarsInRow :+ activeRowVar
        val coeffs = Seq.fill(activeCellVarsInRow.size)(1d) :+ (-2d)
        ilpSolver.addConsBasicLinear("activeVarImpliesAtLeastTwo", vars, coeffs, 0d, largeDbl)
      }

      table.titleRow.indices.foreach { colIdx =>
        val activeColVar = activeColVars((tableIdx, colIdx))
        val activeCellVarsInCol = table.contentMatrix.indices.map { rowIdx =>
          val cellIdx = CellIdx(tableIdx, rowIdx, colIdx)
          val activeCellVar = activeCellVars(cellIdx)
          activeCellVar
        }
        // non-redundant use of tables: if a col is active, at least ONE of its cells must be
        // active; model as sum(activeCellVarsInCol) >= 1*activeColVar, i.e.,
        // 0 <= sum(activeCellVarsInRow) - 1*activeRowVar
        val vars = activeCellVarsInCol :+ activeColVar
        val coeffs = Seq.fill(activeCellVarsInCol.size)(1d) :+ (-1d)
        ilpSolver.addConsBasicLinear("activeVarImpliesAtLeastOne", vars, coeffs, 0d, largeDbl)
      }

      table.titleRow.indices.foreach { colIdx =>
        val activeColVar = activeColVars((tableIdx, colIdx))
        val activeTitleVar = activeTitleVars((tableIdx, colIdx))
        // if title is active, column must be active
        // otherwise we don't need, i.e.: activeTitleVar <= activeColVar
        ilpSolver.addConsXLeqY("activeTitle", activeTitleVar, activeColVar)
      }

      table.titleRow.indices.foreach { colIdx =>
        // for any title var, it is active, if there is any alignment to it, from any Q constituents
        val titleIdx = TitleIdx(tableIdx, colIdx)
        val activeTitleVar = activeTitleVars((tableIdx, colIdx))
        val extAlignmentVarsForTitle = titleToExtAlignmentVars.getOrElse(titleIdx, Seq.empty)
        val vars = extAlignmentVarsForTitle :+ activeTitleVar
        val coeffs = Seq.fill(extAlignmentVarsForTitle.size)(1d) :+ (-1d)
        ilpSolver.addConsBasicLinear("activeTitleImpliesAlignments", vars, coeffs, 0d, largeDbl)
        extAlignmentVarsForTitle.foreach {
          ilpSolver.addConsXLeqY("activeTitle", _, activeTitleVar)
        }
      }
    }

    // if the question choice is active, there is at least one ative thing connected to it.
    // i.e. Sum(incomingToChoice) <= ChoiceVariable
    question.choices.indices.foreach { choiceIdx =>
      val choiceVar = ativeChoiceVars(choiceIdx)
      val extChoiceToExtAlignmentVars = choiceToExtAlignmentVars.getOrElse(choiceIdx, Seq.empty)
      val vars = extChoiceToExtAlignmentVars :+ choiceVar
      val coeffs = Seq.fill(extChoiceToExtAlignmentVars.size)(1d) :+ (-1d)
      ilpSolver.addConsBasicLinear("activeTitleImpliesAlignments", vars, coeffs, 0d, largeDbl)
      // activate the choice variables, whens there is anything aligned to them
      // for any cell connected to the choice: cell <= choice
      extChoiceToExtAlignmentVars.foreach {
        ilpSolver.addConsXLeqY("choiceActivation", _, choiceVar)
      }
    }

    // make sure only one choice is choosen
    val choiceVars = question.choices.indices.map { ativeChoiceVars(_) }
    ilpSolver.addConsBasicSetpack("atMostOneChoice", choiceVars)

    // return all variables
    AllVariables(intraTableVariables, interTableVariables, questionTableVariables,
      questionTitleVariables, qChoiceTableVariables)
  }

  /** An internal method to create an intra-table variable */
  val enableIntraTableVars = false
  private def addIntraTableVariable(
    tableIdx: Int, rowIdx: Int, colIdx1: Int, colIdx2: Int
  ): Option[IntraTableVariable] = {
    if (enableIntraTableVars) {
      val name = s"T=$tableIdx-R=$rowIdx-C1=$colIdx1-C2=$colIdx2"
      val objCoeff = 1d //TODO correct this with alignment score
      val variable = ilpSolver.createBinaryVar(name, objCoeff)
      ilpSolver.addVar(variable)
      Some(IntraTableVariable(tableIdx, rowIdx, colIdx1, colIdx2, variable))
    } else {
      None
    }
  }

  /** An internal method to create an inter-table variable, if there is sufficient lexical match */
  private def addInterTableVariable(
    tableIdx1: Int, tableIdx2: Int, rowIdx1: Int, rowIdx2: Int, colIdx1: Int, colIdx2: Int
  ): Option[InterTableVariable] = {
    val objCoeff = aligner.cell_cell(
      tables(tableIdx1).contentMatrix(rowIdx1)(colIdx1),
      tables(tableIdx2).contentMatrix(rowIdx2)(colIdx2)
    )
    if (objCoeff < minCellCellAlignmentThreshold) {
      None
    } else {
      val name = s"T1=$tableIdx1-T2=$tableIdx2-R1=$rowIdx1-R2=$rowIdx2-C1=$colIdx1-" +
        s"C2=$colIdx2"
      val variable = ilpSolver.createBinaryVar(name, objCoeff)
      ilpSolver.addVar(variable)
      Some(InterTableVariable(tableIdx1, tableIdx2, rowIdx1, rowIdx2, colIdx1, colIdx2, variable))
    }
  }

  /** An internal method to create a question-to-table variable */
  private def addQuestionTableVariable(
    qCons: String, qConsIdx: Int, tableIdx: Int, rowIdx: Int, colIdx: Int
  ): Option[QuestionTableVariable] = {
    val objCoeff = aligner.cell_qCons(tables(tableIdx).contentMatrix(rowIdx)(colIdx), qCons)
    if (objCoeff < minCellQConstituentAlignmentThreshold) {
      None
    } else {
      val name = s"T=$tableIdx-Con=$qConsIdx-R=$rowIdx-C=$colIdx"
      val variable = ilpSolver.createBinaryVar(name, objCoeff)
      ilpSolver.addVar(variable)
      Some(QuestionTableVariable(qConsIdx, tableIdx, rowIdx, colIdx, variable))
    }
  }

  /** An internal method to create a question-to-title variable */
  private def addQuestionTitleVariable(
    qCons: String, qConsIdx: Int, tableIdx: Int, colIdx: Int
  ): Option[QuestionTitleVariable] = {
    val objCoeff = aligner.title_qCons(tables(tableIdx).titleRow(colIdx), qCons)
    if (objCoeff < minTitleQConstituentAlignmentThreshold) {
      None
    } else {
      val name = s"T=$tableIdx-Title=$qConsIdx-C=$colIdx"
      val variable = ilpSolver.createBinaryVar(name, objCoeff)
      ilpSolver.addVar(variable)
      Some(QuestionTitleVariable(qConsIdx, tableIdx, colIdx, variable))
    }
  }

  /** An internal method to create a option-to-table variable */
  private def addQChoiceTableVariable(
    qChoiceCons: String, qConsIdx: Int, tableIdx: Int, rowIdx: Int, colIdx: Int
  ): Option[QuestionTableVariable] = {
    val objCoeff = aligner.cell_qCons(tables(tableIdx).contentMatrix(rowIdx)(colIdx), qChoiceCons)
    if (objCoeff < minCellQConstituentAlignmentThreshold) {
      None
    } else {
      val name = s"T=$tableIdx-QChoice=$qConsIdx-R=$rowIdx-C=$colIdx"
      val variable = ilpSolver.createBinaryVar(name, objCoeff)
      ilpSolver.addVar(variable)
      Some(QuestionTableVariable(qConsIdx, tableIdx, rowIdx, colIdx, variable))
    }
  }

  /** The main method to build an ILP model for a question.
    *
    * @param question a question
    * @return an array of (a subset of) variables of interest whose values may be queried later
    */
  def buildModel(question: Question): AllVariables = {
    // set this up as a maximization problem
    ilpSolver.setAsMaximization()

    // build the question independent part of the model
    val allVarsQuestionIndependent = buildQuestionIndependentModel

    // add the question dependent part of the model; allVars is guaranteed to be a superset of
    // allVarsQuestionIndependent
    val allVars = buildQuestionDependentModel(question, allVarsQuestionIndependent)

    // return all variables
    allVars
  }
}

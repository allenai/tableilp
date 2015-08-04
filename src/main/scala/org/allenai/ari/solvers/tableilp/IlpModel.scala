package org.allenai.ari.solvers.tableilp

import org.allenai.ari.solvers.tableilp.ilpsolver.ScipInterface
import org.allenai.ari.solvers.tableilp.params.{ IlpParams, IlpWeights }
import org.allenai.common.Logging

/** The ILP model for Table Inference.
  *
  * @param ilpSolver a ScipInterface object; safest to create a new instance of it per question
  * @param tables a seq of tables as the knowledge base
  * @param aligner a blackbox to align textual mentions
  * @param ilpParams various parameters for the ILP model
  * @param weights various weights for the ILP model
  */
class IlpModel(
    ilpSolver: ScipInterface,
    tables: Seq[Table],
    aligner: AlignmentFunction,
    ilpParams: IlpParams,
    weights: IlpWeights
) extends Logging {

  /** An index into a cell in a table */
  private case class CellIdx(tableIdx: Int, rowIdx: Int, colIdx: Int)

  /** A title index in a table */
  private case class TitleIdx(tableIdx: Int, colIdx: Int)

  /** A constituent index in a question */
  private case class QuestionIdx(qConsIdx: Int)

  // these set of words in the question text will be ignored before alignment
  private val ignoredWords = Set("?", ":", ",")

  /** The main method to build an ILP model for a question.
    *
    * @param question a question
    * @return a seq of (a subset of) variables of interest whose values may be queried later
    */
  def buildModel(question: TableQuestion): AllVariables = {
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

  /** Auxiliary variables: whether a cell within a given table is "active" */
  private val activeCellVars: Map[CellIdx, Long] = (for {
    tableIdx <- tables.indices
    table = tables(tableIdx)
    rowIdx <- table.contentMatrix.indices
    row = table.contentMatrix(rowIdx)
    colIdx <- row.indices
    x = ilpSolver.createBinaryVar(
      s"activeCell_t=${tableIdx}_r=${rowIdx}_c=$colIdx",
      weights.activeCellObjCoeff
    )
  } yield {
    ilpSolver.addVar(x)
    CellIdx(tableIdx, rowIdx, colIdx) -> x
  }).toMap

  /** Auxiliary variables: whether a row within a given table is "active" */
  private val activeRowVars: Map[(Int, Int), Long] = (for {
    tableIdx <- tables.indices
    table = tables(tableIdx)
    rowIdx <- table.contentMatrix.indices
    x = ilpSolver.createBinaryVar(
      s"activeRow_t=${tableIdx}_r=$rowIdx",
      weights.activeRowObjCoeff
    )
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
    // prefer larger fraction of columns matching
    objCoeff = weights.activeColObjCoeff / table.contentMatrix.head.indices.size
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
    x = ilpSolver.createBinaryVar(
      s"activeTitle_t=${tableIdx}_r=$colIdx",
      weights.activeTitleObjCoeff
    )
  } yield {
    ilpSolver.addVar(x)
    (tableIdx, colIdx) -> x
  }).toMap

  /** Auxiliary variables: whether a table is "active" */
  private val activeTableVars: Map[Int, Long] = (for {
    tableIdx <- tables.indices
    x = ilpSolver.createBinaryVar(s"activeTable_t=$tableIdx", weights.activeTableObjCoeff)
  } yield {
    ilpSolver.addVar(x)
    tableIdx -> x
  }).toMap

  /** The main method to build the question independent aspects of the ILP model.
    * Note: using 'val' rather than 'def' makes this be computed only once.
    *
    * @return a seq of (a subset of) variables of interest whose values may be queried later
    */
  private val buildQuestionIndependentModel: AllVariables = {
    // Intra-table variables
    val intraTableVariables = for {
      tableIdx <- tables.indices
      table = tables(tableIdx)
      rowIdx <- table.contentMatrix.indices
      row = table.contentMatrix(rowIdx)
      colIdx1 <- row.indices
      colIdx2 <- colIdx1 + 1 until row.length
      x <- addIntraTableVariable(tableIdx, rowIdx, colIdx1, colIdx2)
    } yield x

    // Inter-table variables
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
      if (aligner.scoreTitleTitle(table1Entry, table2Entry) < weights.minTitleTitleAlignment) {
        // no good alignment between the titles; disallow inter table alignment
        ilpSolver.chgVarUb(entry.variable, 0d)
      }
    }

    // row activity constraint: allow at most 1 row per table to be active, unless otherwise
    // specified in the configuration
    // Note: question dependent variables may also make the row active; this constraint will
    // take that into account as well
    tables.indices.foreach { tableIdx =>
      val table = tables(tableIdx)
      val tableRowVars = table.contentMatrix.indices.map(r => activeRowVars((tableIdx, r)))
      val ub = ilpParams.maxRowsPerTable
      ilpSolver.addConsAtMostK(s"atMost${ub}Rows_T=$tableIdx", tableRowVars, ub)
      // table activity constraints:
      // (a) if a row is active, then the table is active (NOTE: if title is active, then a row must
      // be active, and then this constraint makes the table active as well);
      // (b) if the table is active, then at least one row is active
      val activeTableVar = activeTableVars(tableIdx)
      tableRowVars.foreach { activeRowVar =>
        ilpSolver.addConsXLeqY(s"activeRowImpliesActiveTable_T=$tableIdx", activeRowVar,
          activeTableVar)
      }
      ilpSolver.addConsYImpliesAtLeastK(s"activeRowsImpliesActiveTable_T=$tableIdx", activeTableVar,
        tableRowVars, 1d)
    }

    // add question independent activity constraints
    tables.indices.foreach { tableIdx =>
      val table = tables(tableIdx)
      table.contentMatrix.indices.foreach { rowIdx =>
        val activeRowVar = activeRowVars((tableIdx, rowIdx))
        val row = table.contentMatrix(rowIdx)
        val activeCellVarsInRow = row.indices.map { colIdx =>
          val cellIdx = CellIdx(tableIdx, rowIdx, colIdx)
          val activeCellVar = activeCellVars(cellIdx)
          val activeColVar = activeColVars((tableIdx, colIdx))
          // if any activeCellVar for a row is 1, make the corresponding activeRowVar be 1
          ilpSolver.addConsXLeqY("activeRow", activeCellVar, activeRowVar)
          ilpSolver.addConsXLeqY("activeCol", activeCellVar, activeColVar)
          // return the active cell var
          activeCellVar
        }
        // non-redundant use of tables: if a row is active, at least TWO of its cells must be
        // active; model as sum(activeCellVarsInRow) >= 2*activeRowVar, i.e.,
        // 0 <= sum(activeCellVarsInRow) - 2*activeRowVar
        ilpSolver.addConsYImpliesAtLeastK("activeRowImpliesAtLeastTwoCells", activeRowVar,
          activeCellVarsInRow, 2d)
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
        ilpSolver.addConsYImpliesAtLeastK("activeColImpliesAtLeastOneCell", activeColVar,
          activeCellVarsInCol, 1d)
      }

      table.titleRow.indices.foreach { colIdx =>
        val activeColVar = activeColVars((tableIdx, colIdx))
        val activeTitleVar = activeTitleVars((tableIdx, colIdx))
        // if title is active, column must be active
        // otherwise we don't need, i.e.: activeTitleVar <= activeColVar
        ilpSolver.addConsXLeqY("activeTitle", activeTitleVar, activeColVar)
      }
    }

    // at most k tables may be active
    val maxTables = ilpParams.maxTablesToChain
    ilpSolver.addConsAtMostK(s"atMost${maxTables}Tables", activeTableVars.values.toSeq, maxTables)

    // return all variables
    AllVariables(intraTableVariables, interTableVariables,
      IndexedSeq.empty, IndexedSeq.empty, IndexedSeq.empty, IndexedSeq.empty)
  }

  /** The main method to build the question dependent aspects of the ILP model.
    *
    * @param question a question
    * @param existingAllVars all variables currently in existence
    * @return a seq of (a subset of) variables of interest whose values may be queried later;
    * this will be a superset of existingAllVars
    */
  private def buildQuestionDependentModel(
    question: TableQuestion, existingAllVars: AllVariables
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
    val qChoiceTitleVariables = for {
      tableIdx <- tables.indices
      qChoiceIdx <- question.choices.indices
      qChoiceCons = question.choices(qChoiceIdx)
      table = tables(tableIdx)
      colIdx <- table.titleRow.indices
      x <- addQChoiceTitleVariable(qChoiceCons, qChoiceIdx, tableIdx, colIdx)
    } yield x

    // Auxiliary variables: whether an answer choice is aligned to something
    val activeChoiceVars: Map[Int, Long] = (for {
      choiceIdx <- question.choices.indices
      x = ilpSolver.createBinaryVar(s"choice=$choiceIdx", weights.activeChoiceObjCoeff)
    } yield {
      ilpSolver.addVar(x)
      choiceIdx -> x
    }).toMap

    // Auxiliary variables: whether a constituent of a given question is "active"
    val activeQuestionVars: Map[Int, Long] = (for {
      qConsIdx <- question.questionCons.indices
      x = ilpSolver.createBinaryVar(s"activeQuestion_t=$qConsIdx", weights.activeQConsObjCoeff)
    } yield {
      ilpSolver.addVar(x)
      qConsIdx -> x
    }).toMap

    // A convenient map from a cell to intra-table ILP variables associated with it
    val tmpIntraTriples = intraTableVariables.flatMap {
      case IntraTableVariable(tableIdx, rowIdx, colIdx1, colIdx2, x) =>
        val cellIdx1 = CellIdx(tableIdx, rowIdx, colIdx1)
        val cellIdx2 = CellIdx(tableIdx, rowIdx, colIdx2)
        Seq(cellIdx1 -> x, cellIdx2 -> x)
    }
    // A convenient map from a cell to inter-table ILP variables associated with it
    val tmpInterTriples = interTableVariables.flatMap {
      case InterTableVariable(tableIdx1, tableIdx2, rowIdx1, rowIdx2, colIdx1, colIdx2, x) =>
        val cellIdx1 = CellIdx(tableIdx1, rowIdx1, colIdx1)
        val cellIdx2 = CellIdx(tableIdx2, rowIdx2, colIdx2)
        Seq(cellIdx1 -> x, cellIdx2 -> x)
    }
    // A convenient map from a cell to question-table ILP variables associated with it
    val tmpQuestionTriples = questionTableVariables.map {
      case QuestionTableVariable(_, tableIdx, rowIdx, colIdx, x) =>
        CellIdx(tableIdx, rowIdx, colIdx) -> x
    }
    // A convenient map from a cell to question-choice ILP variables associated with it
    val tmpChoicesTriples = qChoiceTableVariables.map {
      case ChoiceTableVariable(_, tableIdx, rowIdx, colIdx, x) =>
        CellIdx(tableIdx, rowIdx, colIdx) -> x
    }

    // Collect all external alignment variables per cell; note: simply doing
    // cellToInterTableVars ++ cellToInterTableVars may not work
    val cellToExtAlignmentVars = Utils.toMapUsingGroupByFirst(
      tmpInterTriples ++ tmpQuestionTriples ++ tmpChoicesTriples
    )

    // Collect all external alignments per answer choice
    val tmpChoiceToTableVars = qChoiceTableVariables.map {
      case ChoiceTableVariable(qChoiceCons, _, _, _, x) =>
        qChoiceCons -> x
    }
    val tmpChoiceToTitleVars = qChoiceTitleVariables.map {
      case ChoiceTitleVariable(qChoiceCons, _, _, x) =>
        qChoiceCons -> x
    }
    val choiceToExtAlignmentVars = Utils.toMapUsingGroupByFirst(tmpChoiceToTableVars ++
      tmpChoiceToTitleVars)

    // Collect all external alignments per title
    val tmpTitleToQuestionVars = questionTitleVariables.map {
      case QuestionTitleVariable(_, tableIdx, colIdx, x) =>
        TitleIdx(tableIdx, colIdx) -> x
    }
    val tmpTitleToChoiceVars = qChoiceTitleVariables.map {
      case ChoiceTitleVariable(_, tableIdx, colIdx, x) =>
        TitleIdx(tableIdx, colIdx) -> x
    }
    val titleToExtAlignmentVars = Utils.toMapUsingGroupByFirst(tmpTitleToQuestionVars ++
      tmpTitleToChoiceVars)

    // Collect all external alignments per question constituent
    val tmpQuestionToTitleVars = questionTitleVariables.map {
      case QuestionTitleVariable(qConsIdx, _, _, x) =>
        QuestionIdx(qConsIdx) -> x
    }
    val tmpQuestionToTableVars = questionTableVariables.map {
      case QuestionTableVariable(qConsIdx, _, _, _, x) =>
        QuestionIdx(qConsIdx) -> x
    }
    val questionToExtAlignmentVars = Utils.toMapUsingGroupByFirst(tmpQuestionToTableVars ++
      tmpQuestionToTitleVars)

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
          ilpSolver.addConsYImpliesAtLeastK(
            "activeCellImpliesAtLeastOneExt",
            activeCellVar, extAlignmentVarsForCell, 1d
          )
          // return the active cell var
          activeCellVar
        }
      }

      table.titleRow.indices.foreach { colIdx =>
        // for any title var, it is active, if there is any alignment to it, from any Q constituents
        val titleIdx = TitleIdx(tableIdx, colIdx)
        val activeTitleVar = activeTitleVars((tableIdx, colIdx))
        val extAlignmentVarsForTitle = titleToExtAlignmentVars.getOrElse(titleIdx, Seq.empty)
        ilpSolver.addConsYImpliesAtLeastK("activeTitleImpliesAlignments", activeTitleVar,
          extAlignmentVarsForTitle, 1d)
        extAlignmentVarsForTitle.foreach {
          ilpSolver.addConsXLeqY("activeTitle", _, activeTitleVar)
        }
      }
    }

    // if the question choice is active, there is at least one active thing connected to it.
    // i.e. ChoiceVariable <= Sum(incomingToChoice)
    question.choices.indices.foreach { choiceIdx =>
      val choiceVar = activeChoiceVars(choiceIdx)
      val extAlignmentVarsForChoice = choiceToExtAlignmentVars.getOrElse(choiceIdx, Seq.empty)
      ilpSolver.addConsYImpliesAtLeastK("activeChoiceImpliesAlignments", choiceVar,
        extAlignmentVarsForChoice, 1d)
      // activate the choice variables if there is anything aligned to it: alignedCell => Choice
      extAlignmentVarsForChoice.foreach {
        ilpSolver.addConsXLeqY("choiceActivation", _, choiceVar)
      }
    }

    // a choice should align to at most one cell
    choiceToExtAlignmentVars.values.foreach { extAlignmentVarsForChoice =>
      ilpSolver.addConsAtMostOne("choiceAlignsToAtMostOneCell", extAlignmentVarsForChoice)
    }

    // align at least one question constituent
    val qConsVars = question.questionCons.indices.map(activeQuestionVars)
    ilpSolver.addConsAtLeastOne("atLeastOneQCons", qConsVars)

    // select at most one answer choice
    val choiceVars = question.choices.indices.map(activeChoiceVars)
    ilpSolver.addConsAtMostOne("atMostOneChoice", choiceVars)

    // (optional) select at least one answer choice
    if (ilpParams.mustChooseAnAnswer) ilpSolver.addConsAtLeastOne("atLeastOneChoice", choiceVars)

    // active question variables
    question.questionCons.indices.foreach { qIdx =>
      val questionIdx = QuestionIdx(qIdx)
      val activeQuestionVar = activeQuestionVars(qIdx)
      val extAlignmentVars = questionToExtAlignmentVars.getOrElse(questionIdx, Seq.empty)
      ilpSolver.addConsYImpliesAtLeastK("activeQuestionImpliesAlignments", activeQuestionVar,
        extAlignmentVars, 1d)
      extAlignmentVars.foreach {
        ilpSolver.addConsXLeqY("activeQuestionCons", _, activeQuestionVar)
      }
    }

    // return all variables
    AllVariables(intraTableVariables, interTableVariables, questionTableVariables,
      questionTitleVariables, qChoiceTableVariables, qChoiceTitleVariables)
  }

  /** An internal method to create an intra-table variable */
  private val enableIntraTableVars = false // TODO(ashish33) experiment to determine usefulness
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
      None // intra-table variables not enabled
    }
  }

  /** An internal method to create an inter-table variable, if there is sufficient lexical match */
  private def addInterTableVariable(
    tableIdx1: Int, tableIdx2: Int, rowIdx1: Int, rowIdx2: Int, colIdx1: Int, colIdx2: Int
  ): Option[InterTableVariable] = {
    val objCoeff = aligner.scoreCellCell(
      tables(tableIdx1).contentMatrix(rowIdx1)(colIdx1),
      tables(tableIdx2).contentMatrix(rowIdx2)(colIdx2)
    )
    if (objCoeff < weights.minCellCellAlignment) {
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
    val objCoeff = aligner.scoreCellQCons(tables(tableIdx).contentMatrix(rowIdx)(colIdx), qCons)
    if (objCoeff < weights.minCellQConsAlignment) {
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
    val objCoeff = aligner.scoreTitleQCons(tables(tableIdx).titleRow(colIdx), qCons)
    if (objCoeff < weights.minTitleQConsAlignment) {
      None
    } else {
      val name = s"T=$tableIdx-Title=$qConsIdx-C=$colIdx"
      val variable = ilpSolver.createBinaryVar(name, objCoeff)
      ilpSolver.addVar(variable)
      Some(QuestionTitleVariable(qConsIdx, tableIdx, colIdx, variable))
    }
  }

  /** An internal method to create a question choice-to-title variable */
  private def addQChoiceTitleVariable(
    qChoice: String, qChoiceIdx: Int, tableIdx: Int, colIdx: Int
  ): Option[ChoiceTitleVariable] = {
    val objCoeff = aligner.scoreTitleQChoice(tables(tableIdx).titleRow(colIdx), qChoice)
    if (objCoeff < weights.minTitleQChoiceAlignment) {
      None
    } else {
      val name = s"T=$tableIdx-ChoiceIdx=$qChoiceIdx-C=$colIdx"
      val variable = ilpSolver.createBinaryVar(name, objCoeff)
      ilpSolver.addVar(variable)
      Some(ChoiceTitleVariable(qChoiceIdx, tableIdx, colIdx, variable))
    }
  }

  /** An internal method to create a option-to-table variable */
  private def addQChoiceTableVariable(
    qChoice: String, qChoiceIdx: Int, tableIdx: Int, rowIdx: Int, colIdx: Int
  ): Option[ChoiceTableVariable] = {
    val objCoeff = aligner.scoreCellQChoice(
      tables(tableIdx).contentMatrix(rowIdx)(colIdx),
      qChoice
    )
    if (objCoeff < weights.minCellQChoiceAlignment) {
      None
    } else {
      val name = s"T=$tableIdx-QChoice=$qChoiceIdx-R=$rowIdx-C=$colIdx"
      val variable = ilpSolver.createBinaryVar(name, objCoeff)
      ilpSolver.addVar(variable)
      Some(ChoiceTableVariable(qChoiceIdx, tableIdx, rowIdx, colIdx, variable))
    }
  }
}

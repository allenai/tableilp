package org.allenai.ari.solvers.tableilp

/** All non-auxiliary variables in the ILP model.
  *
  * @param intraTableVariables variables involving two cells in the same table
  * @param interTableVariables variables involving two cells in different tables
  * @param questionTableVariables variables involving a question constituent and a table cell
  * @param questionTitleVariables variables involving a question constituent and a table title
  * @param qChoiceConsTableVariables variables involving an answer choice and a table cell
  * @param qChoiceConsTitleVariables variables involving an answer choice and a table title
  * @param activeChoiceVars variables indicating whether a choice was active, i.e., selected
  */
case class AllVariables(
    intraTableVariables: IndexedSeq[IntraTableVariable],
    interTableVariables: IndexedSeq[InterTableVariable],
    questionTableVariables: IndexedSeq[QuestionTableVariable],
    questionTitleVariables: IndexedSeq[QuestionTitleVariable],
    qChoiceConsTableVariables: IndexedSeq[ChoiceConsTableVariable],
    qChoiceConsTitleVariables: IndexedSeq[ChoiceConsTitleVariable],
    relationVariables: IndexedSeq[RelationMatchVariable],
    activeChoiceVars: Map[Int, Long],
    activeChoiceConsVars: Map[(Int, Int), Long]
) {
  def ++(that: AllVariables): AllVariables = {
    AllVariables(
      intraTableVariables ++ that.intraTableVariables,
      interTableVariables ++ that.interTableVariables,
      questionTableVariables ++ that.questionTableVariables,
      questionTitleVariables ++ that.questionTitleVariables,
      qChoiceConsTableVariables ++ that.qChoiceConsTableVariables,
      qChoiceConsTitleVariables ++ that.qChoiceConsTitleVariables,
      relationVariables ++ that.relationVariables,
      activeChoiceVars ++ that.activeChoiceVars,
      activeChoiceConsVars ++ that.activeChoiceConsVars
    )
  }

  lazy val ilpVars: Seq[Long] = {
    intraTableVariables.map(_.variable) ++ interTableVariables.map(_.variable) ++
      questionTableVariables.map(_.variable) ++ questionTitleVariables.map(_.variable) ++
      qChoiceConsTitleVariables.map(_.variable) ++ qChoiceConsTableVariables.map(_.variable) ++
      relationVariables.map(_.variable) ++ activeChoiceVars.values ++ activeChoiceConsVars.values
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

/** Variables involving an answer choice and a table cell.
  *
  * @param qChoiceIdx index identifying a constitute in the question
  * @param tableIdx index identifying a table
  * @param rowIdx index identifying a row in the table
  * @param colIdx index identifying a column in the table
  * @param variable a pointer to the associated ILP variable
  */
case class ChoiceTableVariable(
  qChoiceIdx: Int,
  tableIdx: Int,
  rowIdx: Int,
  colIdx: Int,
  variable: Long
)

/** Variables involving an answer choice and a table title cell.
  *
  * @param qChoiceIdx index identifying a constitute in the question
  * @param tableIdx index identifying a table
  * @param colIdx index identifying a column in the table
  * @param variable a pointer to the associated ILP variable
  */
case class ChoiceTitleVariable(
  qChoiceIdx: Int,
  tableIdx: Int,
  colIdx: Int,
  variable: Long
)

/** Variables involving an answer choice and a table cell.
  *
  * @param qChoiceIdx index identifying a constitute in the question
  * @param tableIdx index identifying a table
  * @param rowIdx index identifying a row in the table
  * @param colIdx index identifying a column in the table
  * @param variable a pointer to the associated ILP variable
  */
case class ChoiceConsTableVariable(
  qChoiceIdx: Int,
  qChoiceConsIdx: Int,
  tableIdx: Int,
  rowIdx: Int,
  colIdx: Int,
  variable: Long
)

/** Variables involving an answer choice and a table title cell.
  *
  * @param qChoiceIdx index identifying a constitute in the question
  * @param tableIdx index identifying a table
  * @param colIdx index identifying a column in the table
  * @param variable a pointer to the associated ILP variable
  */
case class ChoiceConsTitleVariable(
  qChoiceIdx: Int,
  qChoiceConsIdx: Int,
  tableIdx: Int,
  colIdx: Int,
  variable: Long
)

/** Variables indicating potential representation of the relationship between two columns in a table
  * found in the question
  * @param tableIdx table index
  * @param col1Idx column index of arg1 of the relation
  * @param col2Idx column index of arg2 of the relation
  * @param qMatchStart starting character offset of the pattern in the question. Set to -1, if
  * no representation of relation in the question text.
  * @param qMatchEnd ending character offset of the pattern in the question. Set to -1, if
  * no representation of relation in the question text.
  * @param flipped set to true if arg1(arg2) is expected to appear after(before) the pattern
  * @param variable ILP variable
  */
case class RelationMatchVariable(
  tableIdx: Int,
  col1Idx: Int,
  col2Idx: Int,
  qMatchStart: Int,
  qMatchEnd: Int,
  flipped: Boolean,
  variable: Long
)

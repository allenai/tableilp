package org.allenai.ari.solvers.tableilp

/** All non-auxiliary variables in the ILP model.
  *
  * @param intraTableVariables variables involving two cells in the same table
  * @param interTableVariables variables involving two cells in different tables
  * @param questionTableVariables variables involving a question constituent and a table cell
  * @param questionTitleVariables variables involving a question constituent and a table title
  * @param qChoiceTableVariables variables involving an answer choice and a table cell
  * @param qChoiceTitleVariables variables involving an answer choice and a table title
  */
case class AllVariables(
    intraTableVariables: IndexedSeq[IntraTableVariable],
    interTableVariables: IndexedSeq[InterTableVariable],
    questionTableVariables: IndexedSeq[QuestionTableVariable],
    questionTitleVariables: IndexedSeq[QuestionTitleVariable],
    qChoiceTableVariables: IndexedSeq[ChoiceTableVariable],
    qChoiceTitleVariables: IndexedSeq[ChoiceTitleVariable]
) {
  def ++(that: AllVariables): AllVariables = {
    AllVariables(
      intraTableVariables ++ that.intraTableVariables,
      interTableVariables ++ that.interTableVariables,
      questionTableVariables ++ that.questionTableVariables,
      questionTitleVariables ++ that.questionTitleVariables,
      qChoiceTableVariables ++ that.qChoiceTableVariables,
      qChoiceTitleVariables ++ that.qChoiceTitleVariables
    )
  }

  lazy val ilpVars: Seq[Long] = {
    intraTableVariables.map(_.variable) ++ interTableVariables.map(_.variable) ++
      questionTableVariables.map(_.variable) ++ questionTitleVariables.map(_.variable) ++
      qChoiceTitleVariables.map(_.variable) ++ qChoiceTableVariables.map(_.variable)
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

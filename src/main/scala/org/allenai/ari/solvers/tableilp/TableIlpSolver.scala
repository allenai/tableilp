package org.allenai.ari.solvers.tableilp

import org.allenai.ari.models._
import org.allenai.ari.solvers.SimpleSolver
import org.allenai.ari.solvers.common.{ EntailmentService, KeywordTokenizer }
import org.allenai.ari.solvers.tableilp.ilpsolver._
import org.allenai.ari.solvers.tableilp.params.{ IlpParams, IlpWeights, SolverParams }
import org.allenai.common.{ Resource, Version }

import akka.actor.ActorSystem
import com.google.inject.Inject
import spray.json._

import scala.annotation.tailrec
import scala.concurrent.Future

/** An Aristo solver that uses an Integer Linear Programming (ILP) formulation to find the best
  * inference chain of knowledge represented in the form of tables.
  *
  * @param entailmentService service for computing entailment score between two text sequences
  * @param tokenizer keyword tokenizer, also does stemming
  * @param tableInterface interface to access CSV tables from files
  * @param solverParams various high level parameters of the Aristo solver
  * @param scipParams various parameters for the SCIP solver
  * @param ilpParams various parameters for the ILP model
  * @param weights various weights for the ILP model
  * @param actorSystem the actor system
  */
class TableIlpSolver @Inject() (
    entailmentService: EntailmentService,
    tokenizer: KeywordTokenizer,
    tableInterface: TableInterface,
    solverParams: SolverParams,
    scipParams: ScipParams,
    ilpParams: IlpParams,
    weights: IlpWeights
)(implicit actorSystem: ActorSystem) extends SimpleSolver {
  import actorSystem.dispatcher

  override val name = "TableIlp"

  override val version = Version.fromResources("org.allenai.ari", "tableilp-solver")

  /** config: run actual solver or simply generate a random alignement for debugging */
  private val useActualSolver = true

  /** The score to use when the ILP model doesn't find any support for an answer option */
  private val defaultScore = 0d
  private def defaultIlpAnswer(selection: MultipleChoiceSelection) = {
    SimpleAnswer(selection, defaultScore, Some(Map("ilpSolution" -> JsNull)))
  }
  private def defaultIlpAnswerWithScore(selection: MultipleChoiceSelection, score: Double) = {
    SimpleAnswer(selection, score, Some(Map("ilpSolution" -> JsNull)))
  }

  /** For storing a tied choice along with its score */
  private case class TiedChoice(choice: Int, score: Double)

  /** Load science terms from Datastore */
  private val scienceTerms: Set[String] = {
    val source = Utils.getDatastoreFileAsSource(solverParams.scienceTermsDatastoreConfig)
    // get lines that do not start with '#'
    Resource.using(source)(_.getLines().filterNot(_.startsWith("#")).toSet)
  }
  logger.debug(s"Loaded ${scienceTerms.size} science terms")

  /** The entry point for the solver */
  protected[ari] def handleQuestion(question: Question): Future[Seq[SimpleAnswer]] = {
    // Run the solver asynchronously. This will help prevent your system from timing out or
    // freezing up if you send it lots of questions at once.
    if (!question.isMultipleChoice || question.text.isEmpty) {
      // Return an empty response if this isn't a multiple choice question
      Future.successful(Seq.empty[SimpleAnswer])
    } else {
      Future {
        logger.info(s"Question: ${question.rawQuestion}")
        // create an ILP model, solve it recursively to obtain solutions for all the answer choices
        val allSolutions: Seq[IlpSolution] = if (useActualSolver) {
          val tableSelections = tableInterface.getTablesForQuestion(question.rawQuestion)
          val ilpSolver = new ScipInterface("aristo-tableilp-solver", scipParams)
          val aligner = new AlignmentFunction(ilpParams.alignmentType, Some(entailmentService),
            ilpParams.entailmentScoreOffset, tokenizer, solverParams.useRedisCache)
          val ilpModel = new IlpModel(ilpSolver, aligner, ilpParams, weights, tableInterface,
            tableSelections, tokenizer, scienceTerms)
          val questionIlp = TableQuestionFactory.makeQuestion(question, "Tokenize",
            splitAnswerChoices = ilpParams.splitAnswerChoices, ilpParams.whichTermSpan)
          val allVariables = ilpModel.buildModel(questionIlp)
          if (scipParams.ilpExportFile != "") ilpSolver.exportModel(useOriginal = true)
          val tablesUsed = tableSelections.map(ts => tableInterface.allTables(ts.id))
          // solve for all answer choices, obtaining a sequence of solutions
          val solutions = solveForAllAnswerChoices(ilpSolver, ilpModel, allVariables, questionIlp,
            tablesUsed, Seq.empty, Set.empty)
          // free up SCIP data structures
          ilpSolver.free()
          solutions
        } else {
          Seq(IlpSolutionFactory.makeRandomIlpSolution)
        }

        // Only return answers if some solution was found
        if (allSolutions.isEmpty) {
          Seq.empty
        } else {
          val answeredSolutions = allSolutions map { solution =>
            val ilpSolutionJson = solution.toJson
            val bestChoice = solution.bestChoice
            val bestChoiceScore = solution.bestChoiceScore
            val ilpFeaturesOpt = if (solution.solutionQuality.status == IlpStatusOptimal) {
              val ilpFeatures = new IlpFeatures(solution)
              Some(ilpFeatures.featureMap)
            } else {
              // don't generate features if an optimal solution wasn't found, as this is often
              // due to timeouts and can thus make features vary from run to run
              None
            }
            SimpleAnswer(
              question.selections(bestChoice),
              bestChoiceScore,
              Some(Map("ilpSolution" -> ilpSolutionJson)),
              ilpFeaturesOpt
            )
          }
          // Sorting is currently redundant but ensures any future changes to
          // solveForAllAnswerChoices which is not required to return answers sorted by score
          val sortedAnsweredSolutions = answeredSolutions.sortBy(-_.score)
          // Find the selections that were unanswered
          val choicesAnswered = allSolutions.map(_.bestChoice)
          val selectionsUnanswered = question.selections.filterNot(
            sel => choicesAnswered.contains(sel.index)
          )
          sortedAnsweredSolutions ++ selectionsUnanswered.map(defaultIlpAnswer)
        }
      }
    }
  }

  /** Get all solutions for the currently active choices. The caller is responsible for disabling
    * answer choices in the ilpModel. The recursive call in this function should also
    * ensure that.
    *
    * @param ilpSolutionsSoFar the current set of solutions, passed on for tail recursion
    * @param disabledChoices the current set of answer choice indices disabled in the ilpModel
    */
  @tailrec private def solveForAllAnswerChoices(
    ilpSolver: ScipInterface,
    ilpModel: IlpModel,
    allVariables: AllVariables,
    questionIlp: TableQuestion,
    tablesUsed: Seq[Table],
    ilpSolutionsSoFar: Seq[IlpSolution],
    disabledChoices: Set[Int]
  ): Seq[IlpSolution] = {
    ilpSolver.solve()
    // Trust ilpSolver only if it found an optimal solution; suboptimal solutions will likely
    // create repeatability issues
    ilpSolver.getStatus match {
      case IlpStatusOptimal =>
        val (choiceIdx, _) = IlpSolutionFactory.getBestChoice(allVariables, ilpSolver)
        // Solver picks a disabled choice
        if (disabledChoices.contains(choiceIdx)) {
          if (ilpParams.mustChooseAnAnswer) {
            throw new IllegalStateException("Should never return the same choice if " +
              "mustChooseAnswer is set to true")
          }
          logger.error(s"ILP Solver picked a disabled choice: $choiceIdx")
          logger.debug("Not calling solver on other options.")
          ilpSolutionsSoFar
        } else {
          logger.info(s"Selected choice: $choiceIdx")
          val ilpSolution = IlpSolutionFactory.makeIlpSolution(allVariables, ilpSolver,
            questionIlp, tablesUsed, solverParams.fullTablesInIlpSolution)
          // If the number of disabled choices plus the current choice matches the number of
          // choices in the question, solver won't be able to find any further solutions.
          if (questionIlp.choices.size == disabledChoices.size + 1) {
            logger.debug("All choices explored, no further calls needed.")
            ilpSolutionsSoFar :+ ilpSolution
          } else {
            // Reset solution for any future calls to solve
            ilpSolver.resetSolve()
            logger.debug(s"Disabling choice: $choiceIdx")
            ilpModel.disableAnswerChoice(allVariables.activeChoiceVars(choiceIdx))
            // Call the method again with the best choice disabled
            solveForAllAnswerChoices(ilpSolver, ilpModel, allVariables, questionIlp,
              tablesUsed, ilpSolutionsSoFar :+ ilpSolution, disabledChoices + choiceIdx)
          }
        }
      case IlpStatusFeasible | IlpStatusUnknown =>
        logger.debug("No more solutions because IlpStatus = " + ilpSolver.getStatus)
        val ilpSolution = IlpSolutionFactory.makeSimpleIlpSolution(
          ilpSolver,
          solverParams.fullTablesInIlpSolution
        )
        ilpSolutionsSoFar :+ ilpSolution
      case IlpStatusInfeasible =>
        logger.debug("No more solutions because IlpStatus = " + ilpSolver.getStatus)
        ilpSolutionsSoFar
    }
  }
}

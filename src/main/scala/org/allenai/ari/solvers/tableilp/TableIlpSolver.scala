package org.allenai.ari.solvers.tableilp

import org.allenai.ari.models._
import org.allenai.ari.solvers.SimpleSolver
import org.allenai.ari.solvers.common.{ EntailmentService, KeywordTokenizer }
import org.allenai.ari.solvers.lucience.LucienceSolver
import org.allenai.ari.solvers.tableilp.ilpsolver.{ ScipInterface, ScipParams }
import org.allenai.ari.solvers.tableilp.params.{ IlpParams, IlpWeights, SolverParams }
import org.allenai.common.Version

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
    weights: IlpWeights,
    lucienceSolver: LucienceSolver
)(implicit actorSystem: ActorSystem) extends SimpleSolver {
  import actorSystem.dispatcher

  override val name = "TableIlp"

  override val version = Version.fromResources("org.allenai.ari", "tableilp-solver")

  /** config: run actual solver or simply generate a random alignement for debugging */
  private val useActualSolver = true

  // TODO(ashish33) A defaultScore of 0 would have been nicer, but would currently violate the
  // requirement that it must be smaller than scores of all answer choices the solver finds links to
  private val defaultScore = -10d
  private def defaultIlpAnswer(selection: MultipleChoiceSelection) = {
    SimpleAnswer(selection, defaultScore, Some(Map("ilpSolution" -> JsNull)))
  }
  private def defaultIlpAnswerWithScore(selection: MultipleChoiceSelection, score: Double) = {
    SimpleAnswer(selection, score, Some(Map("ilpSolution" -> JsNull)))
  }

  /** For storing a tied choice along with its score */
  private case class TiedChoice(choice: Int, score: Double)

  /** Override SimpleSolver's implementation of solveInternal to allow calling a fallback solver */
  override protected[ari] def solveInternal(request: SolverRequest): Future[SolverResponse] = {
    handleQuestion(request.question) flatMap { simpleAnswers =>
      val completeAnswers = simpleAnswers map {
        case SimpleAnswer(selection, score, analysisOption, features) => {
          val analysis = analysisOption getOrElse { Map.empty }
          SolverAnswer(selection, Analysis(componentId, Some(score), analysis, features))
        }
      }
      // If no answers returned and fallback solver is enabled, call the fallback solver
      if (completeAnswers.isEmpty && solverParams.useFallbackSolver) {
        val fallbackResponse = lucienceSolver.solveInternal(request)
        fallbackResponse.map { response =>
          val compId = if (solverParams.useFallbackSolverCompId) response.solver else componentId
          val features = Map("fallbackSolverUsed" -> 1d)
          SolverResponse(compId, response.answers.map { answer =>
            SolverAnswer(
              answer.selection,
              Analysis(compId, answer.analysis.confidence, answer.analysis.analysis, Some(features))
            )
          })
        }
      } else {
        Future(SolverResponse(componentId, completeAnswers.sorted))
      }
    }
  }

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
          val tableIdsWithScores = tableInterface.getTableIdsForQuestion(question.rawQuestion)
          val ilpSolver = new ScipInterface("aristo-tableilp-solver", scipParams)
          val aligner = new AlignmentFunction(ilpParams.alignmentType, Some(entailmentService),
            ilpParams.entailmentScoreOffset, tokenizer, solverParams.useRedisCache)
          val ilpModel = new IlpModel(ilpSolver, aligner, ilpParams, weights, tableInterface,
            tableIdsWithScores)
          val questionIlp = TableQuestionFactory.makeQuestion(question, "Tokenize")
          val allVariables = ilpModel.buildModel(questionIlp)
          val tablesUsed = tableIdsWithScores.map { case (t, _) => tableInterface.allTables(t) }
          solveForAllAnswerChoices(ilpSolver, ilpModel, allVariables, questionIlp, tablesUsed,
            Seq.empty, Set.empty)
        } else {
          Seq(IlpSolutionFactory.makeRandomIlpSolution)
        }

        val features = Map("fallbackSolverUsed" -> 0d)

        // Only return answers if some solution was found
        if (allSolutions.isEmpty) {
          Seq.empty
        } else {
          val answeredSolutions = (allSolutions map { solution =>
            val ilpSolutionJson = solution.toJson
            val bestChoice = solution.bestChoice
            val bestChoiceScore = solution.bestChoiceScore
            val ilpFeatures = new IlpFeatures(solution)
            SimpleAnswer(
              question.selections(bestChoice),
              bestChoiceScore, // NOTE: unnormalized; may be negative even for a desirable answer
              Some(Map("ilpSolution" -> ilpSolutionJson)),
              Some(features ++ ilpFeatures.featureMap)
            )
            // Sorting is currently redundant but ensures any future changes to
            // solveForAllAnswerChoices which is not required to return answers sorted by score
          }).sortBy(-_.score)
          // Find the selections that were unanswered
          val choicesAnswered = allSolutions.map(_.bestChoice)
          val selectionsUnanswered = question.selections.filterNot(
            sel => choicesAnswered.contains(sel.index)
          )
          answeredSolutions ++ selectionsUnanswered.map(defaultIlpAnswer)
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
    if (ilpSolver.hasSolution) {
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
        logger.debug(s"Found a solution: $choiceIdx")
        val ilpSolution = IlpSolutionFactory.makeIlpSolution(allVariables, ilpSolver,
          questionIlp, tablesUsed)
        // If the number of disabled choices plus the current choice matches the number of
        // choices in the question, solver won't be able to find any further solutions.
        if (questionIlp.choices.size == disabledChoices.size + 1) {
          logger.debug("All choices explored, no further calls needed.")
          ilpSolutionsSoFar :+ ilpSolution
        } else {
          // Reset solution for any future calls to solve
          ilpSolver.resetSolve()
          logger.debug(s"Disabling choice:  $choiceIdx")
          ilpModel.disableAnswerChoice(allVariables.activeChoiceVars(choiceIdx))
          // Call the method again with the best choice disabled
          solveForAllAnswerChoices(ilpSolver, ilpModel, allVariables, questionIlp,
            tablesUsed, ilpSolutionsSoFar :+ ilpSolution, disabledChoices + choiceIdx)
        }
      }
    } else {
      logger.debug("No more solutions!")
      ilpSolutionsSoFar
    }
  }
}

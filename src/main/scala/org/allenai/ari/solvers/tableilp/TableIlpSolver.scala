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

  private val defaultScore = 0d
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
        // create an ILP model, solve it to obtain the best answer choice, along with (optionally)
        // a tied answer choice index for which the ILP has the same score
        val ilpSolutionWithTie: Option[(IlpSolution, Option[TiedChoice])] = if (useActualSolver) {
          val tablesWithScores = tableInterface.getTablesForQuestion(question.rawQuestion)
          val ilpSolver = new ScipInterface("aristo-tableilp-solver", scipParams)
          val aligner = new AlignmentFunction(ilpParams.alignmentType, Some(entailmentService),
            ilpParams.entailmentScoreOffset, tokenizer, solverParams.useRedisCache)
          val ilpModel = new IlpModel(ilpSolver, tablesWithScores, aligner, ilpParams, weights)
          val questionIlp = TableQuestionFactory.makeQuestion(question, "Chunk")
          val allVariables = ilpModel.buildModel(questionIlp)
          ilpSolver.solve()
          if (solverParams.failOnUnansweredQuestions && !ilpSolver.hasSolution) {
            None
          } else {
            val ilpSolution = IlpSolutionFactory.makeIlpSolution(allVariables, ilpSolver,
              questionIlp, tablesWithScores.map(_._1))
            logger.info(s"Best answer choice = ${ilpSolution.bestChoice}")
            val tiedChoiceOpt = if (ilpSolution.bestChoiceScore > 0d && solverParams.checkForTies) {
              logger.info("Checking for a tie")
              ilpSolver.resetSolve()
              ilpModel.disableAnswerChoice(allVariables.activeChoiceVars(ilpSolution.bestChoice))
              ilpSolver.solve()
              if (ilpSolver.hasSolution) {
                val (choiceIdx, score) = IlpSolutionFactory.getBestChoice(allVariables, ilpSolver)
                logger.info(s"Second best answer choice = $choiceIdx")
                if (score >= ilpSolution.bestChoiceScore - solverParams.tieThreshold) {
                  Some(TiedChoice(choiceIdx, score))
                } else {
                  None
                }
              } else {
                None
              }
            } else {
              None
            }
            Some((ilpSolution, tiedChoiceOpt))
          }
        } else {
          Some((IlpSolutionFactory.makeRandomIlpSolution, None))
        }

        val features = Map("fallbackSolverUsed" -> 0d)
        val answersOpt = ilpSolutionWithTie map {
          case (ilpSolution, tiedChoiceOpt) => {
            val ilpSolutionJson = ilpSolution.toJson
            val bestChoice = ilpSolution.bestChoice
            val bestChoiceScore = ilpSolution.bestChoiceScore
            logger.debug(ilpSolutionJson.toString())
            val ilpBestAnswer = SimpleAnswer(
              question.selections(bestChoice),
              bestChoiceScore,
              Some(Map("ilpSolution" -> ilpSolutionJson)),
              Some(features)
            )
            val ilpTiedAnswerOpt = tiedChoiceOpt map {
              case TiedChoice(choiceIdx, score) =>
                defaultIlpAnswerWithScore(question.selections(choiceIdx), score)
            }
            val coveredChoices = Seq(bestChoice) ++ tiedChoiceOpt.map(_.choice)
            val remainingAnswers = question.selections
              .filterNot(s => coveredChoices.contains(s.index)).map(defaultIlpAnswer)
            Seq(ilpBestAnswer) ++ ilpTiedAnswerOpt ++ remainingAnswers
          }
        }
        answersOpt.getOrElse(Seq.empty)
      }
    }
  }
}

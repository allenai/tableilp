package org.allenai.ari.solvers.tableilp

import org.allenai.ari.models.{ MultipleChoiceSelection, Question }
import org.allenai.ari.solvers.SimpleSolver
import org.allenai.ari.solvers.common.{ EntailmentService, KeywordTokenizer }
import org.allenai.ari.solvers.tableilp.ilpsolver.{ ScipInterface, ScipParams }
import org.allenai.ari.solvers.tableilp.params.{ IlpParams, IlpWeights }
import org.allenai.common.Version

import akka.actor.ActorSystem
import com.google.inject.Inject
import com.google.inject.name.Named
import spray.json._

import scala.concurrent.Future

/** An Aristo solver that uses an Integer Linear Programming (ILP) formulation to find the best
  * inference chain of knowledge represented in the form of tables.
  *
  * @param entailmentService service for computing entailment score between two text sequences
  * @param tokenizer keyword tokenizer, also does stemming
  * @param tableInterface interface to access CSV tables from files
  * @param scipParams various parameters for the SCIP solver
  * @param ilpParams various parameters for the ILP model
  * @param weights various weights for the ILP model
  * @param failOnUnansweredQuestions declare question "unanswered" when no answer choice is found
  * @param useFallbackSolver if this solver doesn't answer the question, use a fallback solver
  * @param actorSystem the actor system
  */
class TableIlpSolver @Inject() (
    entailmentService: EntailmentService,
    tokenizer: KeywordTokenizer,
    tableInterface: TableInterface,
    scipParams: ScipParams,
    ilpParams: IlpParams,
    weights: IlpWeights,
    @Named("solver.failOnUnansweredQuestions") failOnUnansweredQuestions: Boolean,
    @Named("solver.useFallbackSolver") useFallbackSolver: Boolean
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
        val ilpSolutionOpt = if (useActualSolver) {
          val tables = tableInterface.getTablesForQuestion(question.rawQuestion)
          val scipSolver = new ScipInterface("aristo-tableilp-solver", scipParams)
          val aligner = new AlignmentFunction(ilpParams.alignmentType, Some(entailmentService),
            ilpParams.entailmentScoreOffset, tokenizer)
          val ilpModel = new IlpModel(scipSolver, tables, aligner, ilpParams, weights)
          val questionIlp = TableQuestionFactory.makeQuestion(question, "Chunk")
          val allVariables = ilpModel.buildModel(questionIlp)
          scipSolver.solve()
          if (failOnUnansweredQuestions && !scipSolver.hasSolution) {
            if (useFallbackSolver) {
              None // TODO(tushar) add Salience Solver as a fallback solver
            } else {
              None
            }
          } else {
            Some(IlpSolutionFactory.makeIlpSolution(allVariables, scipSolver, questionIlp, tables))
          }
        } else {
          Some(IlpSolutionFactory.makeRandomIlpSolution)
        }

        ilpSolutionOpt match {
          case Some(ilpSolution) => {
            val ilpSolutionJson = ilpSolution.toJson
            logger.debug(ilpSolutionJson.toString())
            val ilpBestAnswer = SimpleAnswer(
              question.selections(ilpSolution.bestChoice),
              ilpSolution.bestChoiceScore,
              Some(Map("ilpSolution" -> ilpSolutionJson))
            )
            val otherAnswers = question.selections.filterNot(_.index == ilpSolution.bestChoice)
              .map(defaultIlpAnswer)
            ilpBestAnswer +: otherAnswers
          }
          case None => Seq.empty
        }
      }
    }
  }
}

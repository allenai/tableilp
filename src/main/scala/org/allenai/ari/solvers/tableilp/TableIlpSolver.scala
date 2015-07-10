package org.allenai.ari.solvers.tableilp

import org.allenai.ari.models.MultipleChoiceQuestion
import org.allenai.ari.solvers.SimpleSolver
import org.allenai.ari.solvers.common.{ EntailmentService, KeywordTokenizer }
import org.allenai.common.Version

import akka.actor.ActorSystem
import com.google.inject.Inject
import com.google.inject.name.Named
import spray.json._
import spray.json.DefaultJsonProtocol._

import scala.concurrent.Future

/** An Aristo solver that uses an Integer Linear Programming (ILP) formulation to find the best
  * inference chain of knowledge represented in the form of tables.
  *
  * @param entailmentService service for computing entailment score between two text sequences
  * @param tokenizer keyword tokenizer, also does stemming
  * @param actorSystem the actor system
  */
class TableIlpSolver @Inject() (
    entailmentService: EntailmentService,
    tokenizer: KeywordTokenizer,
    @Named("useEntailment") useEntailment: Boolean
)(implicit actorSystem: ActorSystem) extends SimpleSolver {
  import actorSystem.dispatcher

  override val name = "TableIlp"

  override val version = Version.fromResources("org.allenai.ari", "tableilp-solver")

  /** This is the entry point for your solver. You should generate one SimpleAnswer per
    * MultipleChoiceSelection in the question.
    */
  protected[ari] def handleQuestion(
    question: MultipleChoiceQuestion
  ): Future[Seq[SimpleAnswer]] = {
    // Run the solver asynchronously. This will help prevent your system from timing out or
    // freezing up if you send it lots of questions at once.
    if (question.text.isEmpty) {
      Future.successful(Seq.empty[SimpleAnswer])
    } else {
      Future {
        logger.info(question.toString)

        // TODO(ashish33) Dummy answers for splitting the PR into multiple smaller ones;
        // will be replaced with an actual call to an ILP solver in a PR to follow.
        val answers = for {
          choiceIdx <- question.selections.indices
        } yield {
          SimpleAnswer(
            question.selections(choiceIdx),
            0.0,
            Some(Map("reasoning" -> "this option is not good! ".toJson))
          )
        }

        answers
      }
    }
  }
}

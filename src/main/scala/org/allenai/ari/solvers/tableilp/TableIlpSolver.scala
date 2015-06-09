package org.allenai.ari.solvers.tableilp

import org.allenai.ari.models.{ MultipleChoiceQuestion, MultipleChoiceSelection, SolverAnswer }
import org.allenai.ari.solvers.SimpleSolver
import org.allenai.common.Version

import akka.actor.ActorSystem
import spray.json._
import spray.json.DefaultJsonProtocol._

import scala.concurrent.Future

// TODO: Document your new solver!
class TableIlpSolver( // TODO: Constructor parameters go here.
)(implicit actorSystem: ActorSystem) extends SimpleSolver {
  import actorSystem.dispatcher

  override val name = "TableIlp"

  override val version = Version.fromResources("org.allenai.ari", "tableilp-solver")

  /** This is the entry point for your solver. You should generate one SimpleAnswer per
    * MultipleChoiceSelection in the question.
    */
  protected[ari] def handleQuestion(question: MultipleChoiceQuestion): Future[Seq[SimpleAnswer]] = {
    // Run the solver asynchronously. This will help prevent your system from timing out or freezing
    // up if you send it lots of questions at once.
    Future {
      // This solver will always pick the alphabetically-first answer. You want your solver to be
      // deterministic - that is, it should pick the same answer each time unless your core
      // algorithm or models change. Otherwise, you end up with a lot of noisy changes in the
      // evaluation UI.

      // Sorted list of the selections (each answer option) by the answer text.
      val sortedSelections: Seq[MultipleChoiceSelection] = question.selections sortBy { selection =>
        // Answer should be non-None for all of the test questions. If you're wanting to be
        // extra-sure, you can fall back on the focus.
        selection.answer.getOrElse(selection.focus)
      }

      // Use 'logger' to print things to the log.
      // Methods are 'trace', 'debug', 'info', 'warn', and 'error'.
      // Use "info" for short messages that occur once-per-answer.
      // Use "debug" & "trace" for more frequent messages.
      // Use "warn" and "error" for messages about unexpected behavior.
      logger.info(s"There are ${sortedSelections.size} options to choose from!")

      // Create a scored SimpleAnswer for each. The score should reflect your confidence in the
      // answer - here, we are very sure the first answer is correct, so we give it 1.0 confidence.
      // It's best to try to make this meaningful.
      val bestAnswer = SimpleAnswer(
        sortedSelections.head,
        // The confidence in this answer.
        1.0,
        // This holds any debug information you'd like to save for later.  All of the values will
        // be displayed in the evaluation framework as raw JSON, and saved with everything.
        Some(Map("reasoning" -> "this was alphabetically first in the list".toJson)),
        // This final value is for solvers that will be integrated into the full Aristo system,
        // and contains any numeric features that can be used to train the answer selector. This
        // can stay None until you're ready to permanently integrate this.
        None
      )

      val otherAnswers = for (selection <- sortedSelections.tail) yield {
        SimpleAnswer(
          selection,
          0.0,
          Some(Map("reasoning" -> "this was not alphabetically first".toJson))
        )
      }

      bestAnswer +: otherAnswers
    }
  }
}

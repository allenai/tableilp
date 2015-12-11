package org.allenai.ari.solvers.tableilp

import org.allenai.ari.models.Question
import org.allenai.ari.solvers.common.KeywordTokenizer
import org.allenai.common.Logging
import org.allenai.nlpstack.core.Chunker
import org.allenai.nlpstack.chunk.OpenNlpChunker
import org.allenai.nlpstack.postag.defaultPostagger
import org.allenai.nlpstack.tokenize.defaultTokenizer

/** This class contains the question raw string, its chunked format, and some other processing.
  *
  * @param questionRaw the raw string of the question
  * @param questionCons the question text, chunked into strings
  * @param questionConsOffsets the offsets for the question constituents in questionCons, empty
  * if not available
  * @param choices answer choices
  * @param choicesCons constituents for each choice. If choices are not split, every choice has a
  * sequence with one element - entire choice string
  * @param choicesConsOffsets offsets of the constituents in the choicesCons. Set to empty
  * sequence if offsets are not available or no splitting on choices
  * @param areChoicesSplit set to true if splitting was performed on the choices
  */
case class TableQuestion(
  questionRaw: String,
  questionCons: IndexedSeq[String],
  questionConsOffsets: IndexedSeq[Int],
  choices: IndexedSeq[String],
  choicesCons: IndexedSeq[IndexedSeq[String]],
  choicesConsOffsets: IndexedSeq[IndexedSeq[Int]],
  areChoicesSplit: Boolean = false
)

/** Various ways to build a TableQuestion instance */
object TableQuestionFactory extends Logging {
  private val spaceSep = " ".r
  private val defaultSplittingType = "Tokenize"

  def makeQuestion(questionCons: Seq[String], choices: Seq[String]): TableQuestion = {
    TableQuestion("", questionCons.toIndexedSeq, IndexedSeq.empty, choices.toIndexedSeq,
      choices.toIndexedSeq.map(spaceSep.split(_).toIndexedSeq), IndexedSeq.empty)
  }

  def makeQuestion(questionRaw: String): TableQuestion = {
    TableQuestion(questionRaw, spaceSep.split(questionRaw), IndexedSeq.empty, IndexedSeq.empty,
      IndexedSeq.empty, IndexedSeq.empty)
  }

  def makeQuestion(questionCons: Seq[String]): TableQuestion = {
    TableQuestion("", questionCons.toIndexedSeq, IndexedSeq.empty, IndexedSeq.empty,
      IndexedSeq.empty, IndexedSeq.empty)
  }

  def makeQuestion(aristoQuestion: Question): TableQuestion = {
    val choices = aristoQuestion.selections.map(_.focus).toIndexedSeq
    TableQuestion(aristoQuestion.rawQuestion, spaceSep.split(aristoQuestion.text.get),
      IndexedSeq.empty, choices, choices.map(spaceSep.split(_).toIndexedSeq), IndexedSeq.empty)
  }

  def makeQuestion(aristoQuestion: Question, splittingType: String): TableQuestion = {
    val splitter = splittingType match {
      case "Tokenize" => new TokenSplitter
      case "Chunk" => new ChunkSplitter
      case "SpaceSplit" => new SpaceSplitter
      case _: String =>
        throw new IllegalArgumentException(s"Split type $splittingType not recognized")
    }

    val (tokens, offsets) = splitter.split(aristoQuestion.text.get)
    val choices = aristoQuestion.selections.map(_.focus).toIndexedSeq
    val ((choiceTokens, choiceOffsetsOptions), areSplit) =
      // Only split if all choices should be split on
      if (choices.forall(doSplitOnChoice)) {
        (choices.map(splitter.split).unzip, true)
      } else {
        ((choices.map(Seq(_)), IndexedSeq.fill(choices.size)(None)), false)
      }

    val question = TableQuestion(
      aristoQuestion.rawQuestion,
      tokens.toIndexedSeq,
      offsets.getOrElse(Seq.empty).toIndexedSeq,
      choices,
      choiceTokens.map(_.toIndexedSeq),
      choiceOffsetsOptions.map(_.getOrElse(Seq.empty).toIndexedSeq),
      areChoicesSplit = areSplit
    )
    logger.debug("Question constituents: " + question.questionCons.mkString(",") +
      question.questionConsOffsets.mkString(","))
    question
  }

  /** Returns true, if we should split a given choice string (choice containing conjunction,
    * phase change, purpose relations).
    */
  def doSplitOnChoice(choice: String): Boolean = {
    choice.contains(" to ") ||
      choice.contains(" and ") ||
      choice.contains(" & ") ||
      choice.contains(" for ")
  }
}

/** Various ways of splitting text */
sealed trait Splitter {
  /** Split a string into sequence of tokens and optional offsets for each token
    * @param str input string
    * @return (Sequence of tokens, optional sequence of character start offsets for each token)
    */
  def split(str: String): (Seq[String], Option[Seq[Int]])
}

/** Split text based on tokenization */
private class TokenSplitter(
    val useStemmedKeywordTokenizer: Boolean = false
) extends Splitter {
  def split(str: String): (Seq[String], Option[Seq[Int]]) = {
    if (useStemmedKeywordTokenizer) {
      // TODO(tushar): Return offsets for keyword tokenizer and make the offsets non-optional
      val tokens = KeywordTokenizer.Default.stemmedKeywordTokenize(str)
      (tokens, None)
    } else {
      val tokens = defaultTokenizer.tokenize(str)
      (tokens.map(_.string), Some(tokens.map(_.offset)))
    }
  }
}

/** Split text based on chunking */
private class ChunkSplitter extends Splitter {
  private val chunker = new OpenNlpChunker()
  def split(str: String): (Seq[String], Option[Seq[Int]]) = {
    val tokens = defaultTokenizer.tokenize(str)
    val posTaggedTokens = defaultPostagger.postagTokenized(tokens)
    val chunkedTokens = chunker.chunkPostagged(posTaggedTokens)
    val intervals = Chunker.intervals(chunkedTokens)
    val toks = intervals.map {
      case (_, anInterval) =>
        (
          chunkedTokens.map(_.string).slice(anInterval.start, anInterval.end).mkString(" "),
          chunkedTokens(anInterval.start).offset
        )
    }
    (toks.map(_._1), Some(toks.map(_._2)))
  }
}

/** Split text based on empty space */
private class SpaceSplitter extends Splitter {
  private val spaceSep = " ".r
  def split(str: String): (Seq[String], Option[Seq[Int]]) = (spaceSep.split(str), None)
}

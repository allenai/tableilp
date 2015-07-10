package org.allenai.ari.solvers.tableilp

import org.allenai.ari.models.MultipleChoiceQuestion
import org.allenai.nlpstack.core.Chunker
import org.allenai.nlpstack.chunk.OpenNlpChunker
import org.allenai.nlpstack.postag.defaultPostagger
import org.allenai.nlpstack.tokenize.defaultTokenizer

/** This class contains the question raw string, its chunked format and some other processing on
  * them.
  * @param questionRaw the raw string of the question
  * @param questionCons the question text, chunked into strings
  * @param choices answer choices
  */
case class Question(
  questionRaw: String,
  questionCons: Seq[String],
  choices: Seq[String]
)

/** Various ways to build a Question instance */
object QuestionFactory {
  private val spaceSep = " "
  private val defaultSplittingType = "SpaceSplit"

  def makeQuestion(questionRaw: String): Question = {
    // TODO: consider adding chunker here
    Question(questionRaw, questionRaw.split(spaceSep), Seq.empty)
  }

  def makeQuestion(questionCons: Seq[String]): Question = {
    Question("", questionCons, Seq.empty)
  }

  def makeQuestion(aristoQuestion: MultipleChoiceQuestion): Question = {
    Question(aristoQuestion.rawQuestion, aristoQuestion.text.get.split(spaceSep),
      aristoQuestion.selections.map(_.focus))
  }

  def makeQuestion(aristoQuestion: MultipleChoiceQuestion, splittingType: String): Question = {
    val splitter = splittingType match {
      case "Tokenize" => new TokenSplitter
      case "Chunk" => new ChunkSplitter
      case "SpaceSplit" => new SpaceSplitter
      case _: String =>
        throw new IllegalArgumentException(s"Split type $splittingType not recognized")
    }
    Question(aristoQuestion.rawQuestion, splitter.split(aristoQuestion.text.get),
      aristoQuestion.selections.map(_.focus))
  }
}

/** Various ways of splitting text */
sealed trait Splitter {
  def split(str: String): Seq[String]
}

/** Split text based on tokenization */
private class TokenSplitter extends Splitter {
  def split(str: String): Seq[String] = defaultTokenizer.tokenize(str).map(_.toString())
}

/** Split text based on chunking */
private class ChunkSplitter extends Splitter {
  private val chunker = new OpenNlpChunker()
  def split(str: String): Seq[String] = {
    val tokens = defaultTokenizer.tokenize(str)
    val posTaggedTokens = defaultPostagger.postagTokenized(tokens)
    val chunkedTokens = chunker.chunkPostagged(posTaggedTokens)
    val intervals = Chunker.intervals(chunkedTokens)
    intervals.map {
      case (_, anInterval) =>
        chunkedTokens.map(_.string).slice(anInterval.start, anInterval.end).mkString(" ")
    }
  }
}

/** Split text based on empty space */
private class SpaceSplitter extends Splitter {
  private val spaceSep = " "
  def split(str: String): Seq[String] = str.split(spaceSep)
}

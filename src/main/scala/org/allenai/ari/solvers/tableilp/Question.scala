package org.allenai.ari.solvers.tableilp

import org.allenai.ari.models.MultipleChoiceQuestion
import org.allenai.common.Logging
import org.allenai.nlpstack.core.Chunker
import org.allenai.nlpstack.tokenize.defaultTokenizer
import org.allenai.nlpstack.chunk.OpenNlpChunker
import org.allenai.nlpstack.postag.defaultPostagger

object SplittingType extends Enumeration {
  val Tokenize, Chunk, SpaceSplit = Value
}

/** This class contains the question raw string, its chunked format and some other processing on
  * them.
  * @param questionRaw: the raw string of the question
  * @param questionCons: the question, chunked to strings
  */
class Question(
    questionRaw: String,
    val questionCons: Array[String],
    val choices: Array[String],
    aristoQuestion: MultipleChoiceQuestion,
    splitType: SplittingType.Value
) extends Logging {

  def this(questionRaw: String) {
    // TODO: add chunker here
    this(questionRaw, questionRaw.split(" "), new Array[String](0), null, SplittingType.SpaceSplit)
  }

  def this(questionCons: Array[String]) {
    this("", questionCons, new Array[String](0), null, SplittingType.SpaceSplit)
  }

  def this(aristoQuestion: MultipleChoiceQuestion) {
    this(aristoQuestion.rawQuestion, aristoQuestion.text.get.split(" "),
      aristoQuestion.selections.map(_.focus).toArray, aristoQuestion, SplittingType.SpaceSplit)
  }

  def this(aristoQuestion: MultipleChoiceQuestion, splittingType: SplittingType.Value) {
    this(aristoQuestion.rawQuestion, splitMethods.split(aristoQuestion.text.get, splittingType),
      aristoQuestion.selections.map(_.focus).toArray, aristoQuestion, splittingType)
  }
}

object splitMethods {
  def split(str: String, splittingType: SplittingType.Value): Array[String] = {
    splittingType match {
      case SplittingType.Chunk => chunk(str)
      case SplittingType.Tokenize => tokenize(str)
      case SplittingType.SpaceSplit => str.split(" ")
    }
  }

  private def chunk(str: String): Array[String] = {
    val tokens = defaultTokenizer.tokenize(str)
    val posTaggedTokens = defaultPostagger.postagTokenized(tokens)

    val chunker = new OpenNlpChunker()
    val chunkedTokens = chunker.chunkPostagged(posTaggedTokens)
    val intervals = Chunker.intervals(chunkedTokens)
    intervals.map {
      case (_, anInterval) =>
        chunkedTokens.map(_.string).slice(anInterval.start, anInterval.end).mkString(" ")
    }.toArray
  }

  private def tokenize(str: String): Array[String] = {
    val tokens = defaultTokenizer.tokenize(str)
    tokens.map { tok => tok.string }.toArray
  }
}

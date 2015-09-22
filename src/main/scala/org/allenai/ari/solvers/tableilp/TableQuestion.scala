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
  * @param choices answer choices
  */
case class TableQuestion(
  questionRaw: String,
  questionCons: IndexedSeq[String],
  choices: IndexedSeq[String]
)

/** Various ways to build a TableQuestion instance */
object TableQuestionFactory extends Logging {
  private val spaceSep = " ".r
  private val defaultSplittingType = "SpaceSplit"

  def makeQuestion(questionRaw: String): TableQuestion = {
    TableQuestion(questionRaw, spaceSep.split(questionRaw), IndexedSeq.empty)
  }

  def makeQuestion(questionCons: Seq[String]): TableQuestion = {
    TableQuestion("", questionCons.toIndexedSeq, IndexedSeq.empty)
  }

  def makeQuestion(aristoQuestion: Question): TableQuestion = {
    TableQuestion(aristoQuestion.rawQuestion, spaceSep.split(aristoQuestion.text.get),
      aristoQuestion.selections.map(_.focus).toIndexedSeq)
  }

  def makeQuestion(aristoQuestion: Question, splittingType: String): TableQuestion = {
    val splitter = splittingType match {
      case "Tokenize" => new TokenSplitter
      case "Chunk" => new ChunkSplitter
      case "SpaceSplit" => new SpaceSplitter
      case _: String =>
        throw new IllegalArgumentException(s"Split type $splittingType not recognized")
    }
    val question = TableQuestion(
      aristoQuestion.rawQuestion,
      splitter.split(aristoQuestion.text.get).toIndexedSeq,
      aristoQuestion.selections.map(_.focus).toIndexedSeq
    )
    logger.info("Question constituents: " + question.questionCons.mkString(","))
    question
  }
}

/** Various ways of splitting text */
sealed trait Splitter {
  def split(str: String): Seq[String]
}

/** Split text based on tokenization */
private class TokenSplitter extends Splitter {
  private val useStemmedKeywordTokenizer = true
  def split(str: String): Seq[String] = {
    if (useStemmedKeywordTokenizer) {
      KeywordTokenizer.Default.stemmedKeywordTokenize(str)
    } else {
      defaultTokenizer.tokenize(str).map(_.string)
    }
  }
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
  private val spaceSep = " ".r
  def split(str: String): Seq[String] = spaceSep.split(str)
}

package org.allenai.ari.solvers.tableilp

import org.allenai.ari.solvers.common.{ EntailmentService, KeywordTokenizer }
import org.allenai.common.Logging

import com.medallia.word2vec.Word2VecModel

import java.io.File

object SimilarityType extends Enumeration {
  val Word2Vec, Entailment, WordOverlap = Value
}

class AlignmentFunction(
    alignmentType: SimilarityType.Value,
    entailmentServiceOpt: Option[EntailmentService]
) extends Logging {
  private val word2vecFile = "main/resources/vectors/GoogleNews-vectors-negative300_size=200000.bin"

  // an entailment score below this value is considered to have negative correlation
  private val entailmentScoreOffset = 0.2

  // a stemmer and tokenizer
  private val tokenizer = KeywordTokenizer.Default

  // decide whether to use entailment service or not based on the Option
  private val useEntailment = entailmentServiceOpt.isDefined

  if (entailmentServiceOpt.isDefined) {
    logger.info("Using entailment service for alignment score computation")
  } else {
    logger.info("Using word overlap for alignment score computation")
  }

  /** Alignment score between two titles of tables */
  def title_title(text1: String, text2: String): Double = {
    alignmentType match {
      case SimilarityType.Entailment => getSymmetricScore(text1, text2, getEntailmentScore)
      case SimilarityType.Word2Vec => getWord2VecScore(text1, text2)
      case SimilarityType.WordOverlap => getSymmetricScore(text1, text2, getWordOverlap)
    }
  }

  /** Alignment score betwene cells of two tables */
  def cell_cell(text1: String, text2: String): Double = {
    alignmentType match {
      case SimilarityType.Entailment => getSymmetricScore(text1, text2, getEntailmentScore)
      case SimilarityType.Word2Vec => getWord2VecScore(text1, text2)
      case SimilarityType.WordOverlap => getSymmetricScore(text1, text2, getWordOverlap)
    }
  }

  /** Alignment score between a cell of a table, and a question constituent */
  def cell_qCons(text1: String, text2: String): Double = {
    alignmentType match {
      case SimilarityType.Entailment => getEntailmentScore(text2, text1)
      case SimilarityType.Word2Vec => getWord2VecScore(text2, text1)
      case SimilarityType.WordOverlap => getWordOverlap(text1, text2)
    }
  }

  /** Alignment score between a title of a table, and a question constituent */
  def title_qCons(text1: String, text2: String): Double = {
    alignmentType match {
      case SimilarityType.Entailment => getEntailmentScore(text2, text1)
      case SimilarityType.Word2Vec => getWord2VecScore(text2, text1)
      case SimilarityType.WordOverlap => getWordOverlap(text1, text2)
    }
  }

  // how much does text1 entail text2? (directional)
  private def getEntailmentScore(text1: String, text2: String): Double = {
    assert(entailmentServiceOpt.isDefined, "No entailment service available")
    val text1StemmedTokens = text1.split(';').map(s => tokenizer.stemmedKeywordTokenize(s.trim))
    val text2StemmedTokens = text2.split(';').map(s => tokenizer.stemmedKeywordTokenize(s.trim))
    val scores = for {
      text1StemmedTokens <- text1StemmedTokens
      text2StemmedTokens <- text2StemmedTokens
    } yield {
      entailmentServiceOpt.get.entail(text1StemmedTokens, text2StemmedTokens).confidence -
        entailmentScoreOffset
    }
    scores.max
  }

  // what fraction of text2 words are "covered" by text1 words?
  private def getWordOverlap(text1: String, text2: String): Double = {
    val text1StemmedTokens = tokenizer.stemmedKeywordTokenize(text1)
    val text2StemmedTokens = tokenizer.stemmedKeywordTokenize(text2)
    val coverage = text2StemmedTokens.intersect(text1StemmedTokens).size
    coverage.toDouble / text2StemmedTokens.size
  }

  // cosine distance between two pieces of text
  private lazy val w2vModel = Word2VecModel.fromBinFile(new File(word2vecFile))
  private def getWord2VecScore(text1: String, text2: String): Double = {
    val text1Modified = if (w2vModel.forSearch().contains(text1)) text1 else "</s>"
    val text2Modified = if (w2vModel.forSearch().contains(text2)) text2 else "</s>"
    w2vModel.forSearch().cosineDistance(text1Modified, text2Modified)
  }

  // turn a one-sided score into a symmetric one
  private def getSymmetricScore(text1: String, text2: String,
    scoringFunction: (String, String) => Double): Double = {
    (scoringFunction(text1, text2) + scoringFunction(text2, text1)) / 2d
  }
}

package org.allenai.ari.solvers.tableilp

import org.allenai.common.{ Logging, Resource }

import java.io.{ InputStreamReader, InputStream }

import scala.io.{ BufferedSource, Source }

/** General utilities */
object Utils extends Logging {
  /** A common use case for groupBy. Takes in a sequence of pairs, groups them by the first
    * element, and returns a map from the group identifier to a sequence of second elements
    * of the matching pairs. E.g., ((a,1), (b,2), (a,3), (b,4)) turns into {a -> (1,3),
    * b -> (2,4)}
    */
  def toMapUsingGroupByFirst[T1, T2](x: Seq[(T1, T2)]): Map[T1, Seq[T2]] = {
    x.groupBy(_._1).mapValues(_.unzip._2)
  }

  /** A small value to handle floating point mismatches. */
  val eps = 1e-6

  /** Get a resource file as a Stream */
  def getResourceAsStream(name: String): InputStream = {
    getClass.getClassLoader.getResourceAsStream(name)
  }

  /** Get a resource file as a Reader */
  def getResourceAsReader(name: String): InputStreamReader = {
    new InputStreamReader(getResourceAsStream(name))
  }

  /** Get a resource file as a buffered Source */
  def getResourceAsSource(name: String): BufferedSource = {
    Source.fromInputStream(getResourceAsStream(name))
  }

  /** Get a resource file as a sequence of lines */
  def getResourceAsLines(name: String): Seq[String] = {
    logger.info(s"Loading $name")
    Resource.using(getResourceAsSource(name)) { input =>
      val lines = input.getLines().toVector // convert to vector to force stream to be processed
      logger.trace(s"lines:\n\t${lines.mkString("\n\t")}")
      lines
    }
  }
}

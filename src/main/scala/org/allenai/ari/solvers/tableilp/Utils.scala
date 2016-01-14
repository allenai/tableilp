package org.allenai.ari.solvers.tableilp

import org.allenai.common.{ Logging, Resource }
import org.allenai.datastore.Datastore

import com.typesafe.config.Config

import java.io.{ File, BufferedInputStream, BufferedReader, InputStreamReader }

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

  /** Precision corresponding to eps. */
  val precision = 6

  /** Round a Double to k decimal digits. */
  def round(d: Double, precision: Int): Double = {
    BigDecimal(d).setScale(precision, BigDecimal.RoundingMode.HALF_UP).toDouble
  }

  /** Get a resource file as a Stream */
  def getResourceAsStream(name: String): BufferedInputStream = {
    new BufferedInputStream(getClass.getClassLoader.getResourceAsStream(name))
  }

  /** Get a resource file as a Reader */
  def getResourceAsReader(name: String): BufferedReader = {
    new BufferedReader(new InputStreamReader(getResourceAsStream(name)))
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

  /** Get a datastore file as a buffered Source */
  def getDatastoreFileAsSource(config: Config): BufferedSource = {
    val datastoreName = config.getString("datastore")
    val group = config.getString("group")
    val name = config.getString("name")
    val version = config.getInt("version")
    logger.debug(s"Loading file from $datastoreName datastore: $group/$name-v$version")
    val file = Datastore(datastoreName).filePath(group, name, version).toFile
    Source.fromFile(file)
  }

  /** Get a datastore directory as a folder */
  def getDatastoreDirectoryAsFolder(config: Config): File = {
    val datastoreName = config.getString("datastore")
    val group = config.getString("group")
    val name = config.getString("name")
    val version = config.getInt("version")
    logger.debug(s"Loading directory from $datastoreName datastore: $group/$name-v$version")
    Datastore(datastoreName).directoryPath(group, name, version).toFile
  }
}

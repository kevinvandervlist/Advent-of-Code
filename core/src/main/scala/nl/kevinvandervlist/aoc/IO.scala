package nl.kevinvandervlist.aoc

import scala.io.Source

object IO {
  def resourceByLines(resourceName: String): List[String] =
    Source.fromResource(resourceName).getLines().toList

  def resource(resourceName: String): String =
    Source.fromResource(resourceName).getLines().mkString(System.lineSeparator())
}

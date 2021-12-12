package nl.kevinvandervlist.aoc2021.day12

import scala.annotation.tailrec

object PassagePathing {
  def one(in: List[String]): Int =
    exploreOne(buildGraph(in), List("start")).size

  def two(in: List[String]): Int =
    exploreTwo(buildGraph(in), List("start")).size

  private def buildGraph(in: List[String]): Map[String, Set[String]] = {
    val edges: Iterable[(String, String)] = in.map(_.split('-')).map(a => a(0) -> a(1))
    (edges ++ edges.map(_.swap)).foldLeft(Map.empty[String, Set[String]]) {
      case (g, (from, to)) => g + (from -> (g.getOrElse(from, Set.empty) + to))
    }
  }

  private def exploreOne(graph: Map[String, Set[String]], trace: List[String]): Set[List[String]] = trace match {
    case head :: tail if head == "end" => Set(trace) // done
    case head :: tail if isLowerCase(head) && tail.contains(head) => Set.empty // loop
    case head :: _ => graph(head)
      .flatMap(e => exploreOne(graph, e :: trace))
      .filterNot(_.isEmpty)
  }

  private def exploreTwo(graph: Map[String, Set[String]], trace: List[String]): Set[List[String]] = trace match {
    case head :: tail if head == "end" => Set(trace) // done
    case head :: tail if head == "start" && tail != Nil => Set.empty // done
    case head :: tail if isLowerCase(head) && tail.contains(head) => // accept this occurrence, but jump to old criteria
      graph(head)
        .flatMap(e => exploreOne(graph, e :: trace))
        .filterNot(_.isEmpty)
    case head :: _ => graph(head)
      .flatMap(e => exploreTwo(graph, e :: trace))
      .filterNot(_.isEmpty)
  }

  private def isLowerCase(s: String): Boolean =
    s.toLowerCase == s
}

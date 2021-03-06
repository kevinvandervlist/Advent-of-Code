package nl.kevinvandervlist.aoc2020.day07

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object LuggageProcessing {
  private val my_bag = "shiny gold"
  def one(in: List[String]): Int =
    parseRules(in).eventuallyHoldsAtLeastOne(my_bag)

  def two(in: List[String]): Int =
    parseRules(in).numberOfBagsRequired(my_bag)

  private def parseRules(rules: List[String]): Rules = rules.foldLeft(Rules(Map.empty)) {
    case (rules, line) =>
      // wavy red bags contain 1 light magenta bag, 4 wavy plum bags, 2 vibrant tan bags, 3 dotted turquoise bags.
      val splitted = line.split(Array(' ', ',')).filterNot(_.isEmpty)
      val k = s"${splitted(0)} ${splitted(1)}"
      var idx = 4
      val v = new ListBuffer[Content]
      while(idx < splitted.length) {
        if(splitted(idx) == "no") {
          idx = splitted.length
        } else {
          v.addOne(Content(s"${splitted(idx + 1)} ${splitted(idx + 2)}", splitted(idx).toInt))
          idx += 4
        }
      }
      rules.copy(rel = rules.rel + (k -> v.toList))
  }
}

private case class Rules(rel: Map[String, List[Content]]) {
  def numberOfBagsRequired(name: String): Int = {
    @tailrec
    def rec(idx: Int, stack: List[Content]): Int = stack match {
      case Nil => idx
      case head :: tail =>
        val headContent = (1 to head.amount).flatMap(_ => rel(head.name)).toList
        rec(idx + head.amount, headContent ++ tail)
    }
    rec(0, rel(name))
  }

  def eventuallyHoldsAtLeastOne(name: String): Int = {
    @tailrec
    def rec(seen: Set[String], queue: List[String]): Int = queue match {
      case Nil => seen.size
      case head :: tail => rec(seen ++ Set(head), isContainedIn(head) ++ tail)
    }
    rec(Set.empty, isContainedIn(name))
  }

  // Find which bags the given bag is contained in
  private def isContainedIn(name: String): List[String] = rel
    .filter(_._2.map(_.name).contains(name))
    .keys
    .toList
}

private case class Content(name: String, amount: Int)
package nl.kevinvandervlist.aoc2020.day07

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object LuggageProcessing {
  def one(in: List[String]): Int =
    parseRules(in).eventuallyHoldsAtLeastOne("shiny gold")

  def two(in: List[String]): Int =
    parseRules(in).numberOfBagsRequired("shiny gold")

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
    def numBagsRec(stack: List[Content]): Int = stack match {
      case Nil => 1
      case head :: tail => (head.amount * numBagsRec(rel(head.name))) + numBagsRec(tail)
    }
    numBagsRec(rel(name)) - 1
  }

  def eventuallyHoldsAtLeastOne(name: String): Int = {
    val queue = new mutable.Queue[String]()
    val result = new ListBuffer[String]()

    queue.addAll(isContainedIn(name))

    while(queue.nonEmpty) {
      val p = queue.dequeue()
      result.addOne(p)
      queue.addAll(isContainedIn(p))
    }
    result.toSet.size
  }

  // Find which bags the given bag is contained in
  private def isContainedIn(name: String): List[String] = rel
    .filter(_._2.map(_.name).contains(name))
    .keys
    .toList
}

private case class Content(name: String, amount: Int)
package nl.kevinvandervlist.aoc2020.day16

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object TicketTranslation {
  def one(in: List[String]): Int = {
    val (rules, tickets) = parse(in)
    tickets.map(_.errorRate(rules)).sum
  }

  def two(in: List[String]): Long = {
    val (rules, tickets) = parse(in)
    val myTicket :: nearbyTickets = tickets
    val validTickets = nearbyTickets.filter(_.isValid(rules))
    val identifiedFields = identifyFields(rules, myTicket :: validTickets)
    val departureFields = identifiedFields.filter {
      case (_, v) => v.startsWith("departure")
    }
    val result = departureFields.keys.map(pos => myTicket.numbers(pos).toLong)
    result.product
  }

  private def identifyFields(rules: List[Rule], tickets: List[Ticket]): Map[Int, String] = {
    val maybe: mutable.Map[Int, List[Rule]] = mutable.Map.empty
    // Collect all rules that might be applicable at any position
    var maybeApplicable = true
    for(i <- tickets.head.numbers.indices) {
      for(r <- rules) {
        maybeApplicable = true
        for(t <- tickets) {
          if(! t.validate(i, r)) {
            maybeApplicable = false
          }
        }
        if (maybeApplicable) {
          maybe.put(i, r :: maybe.getOrElse(i, List.empty))
        }
      }
    }

    // Now reduce it such that each position has one rule that's applicable
    def resolve(chosen: List[Map[Int, Rule]], maybe: Map[Int, List[Rule]]): List[Map[Int, Rule]] = {
      if (maybe.isEmpty) {
        return chosen
      }
      // Find the choice with least amount of options to limit complexity
      val choice = maybe.keys.toList.minBy(k => maybe(k).size)
      val remainder = maybe - choice
      val options = maybe(choice).map(r => {
        val _remainder = remainder.map {
          case (x, remainingRules) => x -> remainingRules.filterNot(_ == r)
        }
        if(_remainder.exists(_._2.isEmpty)) {
          return List.empty
        } else {
          resolve(chosen.map(c => c + (choice -> r)), _remainder)
        }
      })
      options.flatten
    }

    val resolved = resolve(List(Map.empty), maybe.toMap)
    resolved.head.map {
      case (k, v) => k -> v.name
    }
  }

  private def parse(in: List[String]): (List[Rule], List[Ticket]) = {
    val rules = new ListBuffer[Rule]
    val tickets = new ListBuffer[Ticket]
    val _rule = "(.*): (\\d+-\\d+) or (\\d+-\\d+)".r
    var idx = 0
    while(_rule.matches(in(idx))) {
      val _rule(name, a, b) = in(idx)
      val _a = a.split('-').map(_.toInt)
      val _b = b.split('-').map(_.toInt)
      rules.append(Rule(
        name, _a(0) -> _a(1), _b(0) -> _b(1)
      ))
      idx += 1
    }

    // Jump to my ticket
    idx += 2
    tickets.addOne(Ticket(
      in(idx).split(',').map(_.toInt).toList
    ))

    // Jump to other tickets
    idx += 3

    while(idx < in.size) {
      tickets.addOne(Ticket(
        in(idx).split(',').map(_.toInt).toList
      ))
      idx += 1
    }

    (rules.toList, tickets.toList)
  }
}

private case class Rule(name: String, fst: (Int, Int), snd: (Int, Int)) {
  def isValid(n: Int): Boolean =
    isValid(fst, n) || isValid(snd, n)

  @inline
  private def isValid(boundary: (Int, Int), n: Int): Boolean =
    boundary._1 <= n && n <= boundary._2
}

private case class Ticket(numbers: List[Int]) {
  def isValid(rules: List[Rule]): Boolean =
    numbers.forall(n => rules.exists(_.isValid(n)))

  def errorRate(rules: List[Rule]): Int = {
    numbers.filter(n => ! rules.exists(_.isValid(n))).sum
  }

  def validate(pos: Int, rule: Rule): Boolean =
    rule.isValid(numbers(pos))
}
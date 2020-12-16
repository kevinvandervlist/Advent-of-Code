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
          if(! t.validateRuleForField(i, r)) {
            maybeApplicable = false
          }
        }
        if (maybeApplicable) {
          maybe.put(i, r :: maybe.getOrElse(i, List.empty))
        }
      }
    }

    resolve(Map.empty, maybe.toMap).head.map {
      case (k, v) => k -> v.name
    }
  }

  // Reduce rule alternatives such that each position has one rule that's applicable
  private def resolve(chosen: Map[Int, Rule], maybe: Map[Int, List[Rule]]): Option[Map[Int, Rule]] = {
    if (maybe.isEmpty) {
      return Some(chosen)
    }
    // Find the choice with least amount of options to limit recursion complexity
    val choice = maybe.keys.minBy(k => maybe(k).size)
    val remainder = maybe - choice
    maybe(choice).iterator.map(r => {
      val updatedRemainder = remainder.map {
        case (x, remainingRules) => x -> remainingRules.filterNot(_ == r)
      }
      // There remains a field that does not have valid options anymore
      if(updatedRemainder.exists(_._2.isEmpty)) {
        return None
      } else {
        resolve(chosen + (choice -> r), updatedRemainder)
      }
    }).collectFirst {
      case Some(x) => x
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
      in(idx).split(',').map(_.toInt).toVector
    ))

    // Jump to other tickets
    idx += 3

    while(idx < in.size) {
      tickets.addOne(Ticket(
        in(idx).split(',').map(_.toInt).toVector
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

private case class Ticket(numbers: Vector[Int]) {
  def isValid(rules: List[Rule]): Boolean =
    numbers.forall(n => rules.exists(_.isValid(n)))

  def errorRate(rules: List[Rule]): Int =
    numbers.filter(n => ! rules.exists(_.isValid(n))).sum

  def validateRuleForField(pos: Int, rule: Rule): Boolean =
    rule.isValid(numbers(pos))
}
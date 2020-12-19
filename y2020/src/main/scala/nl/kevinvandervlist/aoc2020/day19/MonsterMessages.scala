package nl.kevinvandervlist.aoc2020.day19

object MonsterMessages {
  def one(in: List[String]): Int = {
    val (rules, messages) = parseRulesAndMessages(in)
    messages.count(_.isValid(rules.head, rules.map(r => r.no -> r).toMap))
  }

  def two(in: List[String]): Int = {
    val (rules, messages) = parseRulesAndMessages(in)
    val patched = rules.collect {
      case r if r.no == 8 => Rule.apply("8: 42 | 42 8")
      case r if r.no == 11 => Rule.apply("11: 42 31 | 42 11 31")
      case otherwise => otherwise
    }
    messages.count(_.isValid(patched.head, patched.map(r => r.no -> r).toMap))
  }

  private def parseRulesAndMessages(raw: List[String]): (List[Rule], List[Message]) = {
    val rules = raw.takeWhile(_ != "")
    val messages = raw.drop(rules.length + 1)
    (rules.map(Rule.apply).sortBy(_.no), messages.map(Message.apply))
  }
}

private case class Message(line: String) {
  def isValid(selected: Rule, catalog: Map[Int, Rule]): Boolean =
    selected.consume(line.toList, catalog).exists {
      case (valid, remainder) => valid && remainder.isEmpty
    }
}

private object Rule {
  val lit = "(\\d+): \"([a-zA-Z])\"".r
  val serial = "(\\d+): (\\d+)([^\\|]*)".r
  val choice = "(\\d+): (\\d+)([^\\|]*) \\| (\\d+)(.*)".r
  def apply(line: String): Rule = {
    @inline
    def rest(m: String): List[Int] = if(m == null) {
      List.empty
    } else {
      m.split(' ').map(_.trim).filterNot(_.isEmpty).map(_.toInt).toList
    }

    if(lit.matches(line)) {
      val lit(n, l) = line
      assert(l.length == 1)
      return Lit(n.toInt, l.head)
    }
    if(serial.matches(line)) {
      val serial(n, a, t) = line
      return Serial(n.toInt, a.toInt :: rest(t))
    }
    if(choice.matches(line)) {
      val choice(n, a, ar, b, br) = line
      return Choice(n.toInt, a.toInt :: rest(ar), b.toInt :: rest(br))
    }
    ???
  }
}

private sealed trait Rule {
  def consume(tokens: List[Char], catalog: Map[Int, Rule]): List[(Boolean, List[Char])]
  protected def validFold(tokens: List[Char], rules: List[Int], catalog: Map[Int, Rule]): List[(Boolean, List[Char])] = {
    rules.map(catalog.apply).foldLeft(List(true -> tokens)) {
      case (acc, r) => acc.flatMap {
        case (isValid, remainder) =>
          if (isValid) {
            r.consume(remainder, catalog)
          } else {
            List(isValid -> remainder)
          }
      }
    }
  }
  def no: Int
}

private case class Lit(no: Int, literal: Char) extends Rule {
  def consume(tokens: List[Char], catalog: Map[Int, Rule]): List[(Boolean, List[Char])] = tokens match {
    case head :: tail => List((head == literal) -> tail)
    case Nil => List(false -> Nil)
  }
}

private case class Serial(no: Int, rules: List[Int]) extends Rule {
  def consume(tokens: List[Char], catalog: Map[Int, Rule]): List[(Boolean, List[Char])] =
    validFold(tokens, rules, catalog)
}

private case class Choice(no: Int, fst: List[Int], snd: List[Int]) extends Rule {
  override def consume(tokens: List[Char], catalog: Map[Int, Rule]): List[(Boolean, List[Char])] =
    validFold(tokens, fst, catalog) ++ validFold(tokens, snd, catalog)
}

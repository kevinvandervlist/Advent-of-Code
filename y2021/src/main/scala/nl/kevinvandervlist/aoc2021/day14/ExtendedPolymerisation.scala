package nl.kevinvandervlist.aoc2021.day14

object ExtendedPolymerisation {
  def one(in: List[String]): Int =
    count(in.head, substitutes(in.tail.tail), 10).toInt

  def two(in: List[String]): Long =
    count(in.head, substitutes(in.tail.tail), 40)

  private def count(template: String, substs: Map[(Char, Char), List[(Char, Char)]], iterations: Int): Long = {
    // Each key in the map is a two character tuple
    val curGen: Map[(Char, Char), Long] = template.zip(template.tail).foldLeft(Map.empty[(Char, Char), Long]) {
      case (m, (a, b)) => m + ((a -> b) -> (m.getOrElse((a, b), 0L) + 1))
    }

    // apply iterations
    val finalGen = (1 to iterations).foldLeft(curGen) {
      case (cur, _) => cur.foldLeft(Map.empty[(Char, Char), Long]) { // for each generation
        case (nw, (pattern, count)) => substs(pattern).foldLeft(nw) { // and each pattern/count tuple
          case (g, replacement) => g + (replacement -> (g.getOrElse(replacement, 0L) + count)) // update their counts
        }
      }
    }

    // aargh off-by-one error. The head of the template is not included if you just look at the tails of pairs...
    // Nice that the test input not have min/max as head of the template...
    val frequencies = finalGen.foldLeft(Map(template.head -> 1L)) {
      case (m, ((_, c), n)) => m + (c -> (m.getOrElse(c, 0L) + n))
    }

    val sizes = frequencies.values
    sizes.max - sizes.min
  }

  private def substitutes(instructions: List[String]): Map[(Char, Char), List[(Char, Char)]] = {
    val re = "([A-Z])([A-Z]) -> ([A-Z])".r
    instructions.map(s => {
      val re(a, b, c) = s
      a.head -> (b.head -> c.head)
    }).foldLeft(Map.empty[(Char, Char), List[(Char, Char)]]) {
      case (m, (fst, (snd, mid))) => m + ((fst -> snd) -> List((fst, mid), (mid, snd)))
    }
  }
}

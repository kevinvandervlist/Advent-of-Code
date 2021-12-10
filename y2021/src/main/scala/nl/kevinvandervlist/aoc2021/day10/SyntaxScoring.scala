package nl.kevinvandervlist.aoc2021.day10

object SyntaxScoring {
  private val open = Vector('(', '[', '{', '<')
  private val close = Vector(')', ']', '}', '>')
  private val syntaxScoring = Map(')' -> 3, ']' -> 57, '}' -> 1197, '>' -> 25137)
  private val completionScoring = Map(')' -> 1, ']' -> 2, '}' -> 3, '>' -> 4)

  def one(in: List[String]): Long =
    in.map(corruptedScore)
      .filter(_ != 0)
      .sum

  def two (in: List[String]): Long = middle(
    in.filter(l => corruptedScore(l) == 0)
      .map(autoComplete)
      .map(scoreCompletion)
      .sorted
  )

  private def middle(lst: List[Long]): Long =
    lst((lst.length - 1) / 2)

  private def matching(o: Char, c: Char): Boolean =
    matching(o) == c

  private def matching(o: Char): Char =
    close(open.indexOf(o))

  private def analyse(line: String): (Int, List[Char]) = {
    var stack = List.empty[Char]
    for(c <- line.toCharArray) {
      if(open.contains(c)) {
        stack = c :: stack
      } else if(close.contains(c) && matching(stack.head, c)) {
        stack = stack.tail
      } else {
        return syntaxScoring.getOrElse(c, 0) -> stack
      }
    }
    0 -> stack
  }

  private def corruptedScore(line: String): Int =
    analyse(line)._1

  private def autoComplete(line: String): String =
    analyse(line)
      ._2.
      map(matching)
      .mkString("")

  private def scoreCompletion(completion: String): Long =
    completion.toCharArray.foldLeft(0L) {
      case (total, c) => total * 5 + completionScoring(c)
    }
}

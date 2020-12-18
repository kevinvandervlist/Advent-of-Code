package nl.kevinvandervlist.aoc2020.day18

object OperationOrder {
  def one(in: List[String]): Long = in
    .iterator
    .map(tokenize)
    .map(reduce)
    .sum

  def two(in: List[String]): Long = in
    .iterator
    .map(tokenize)
    .map(reduce2)
    .sum

  private def tokenize(expr: String): List[String] = expr
    .replace("(", " ( ")
    .replace(")", " ) ")
    .split(' ')
    .filterNot(_.isEmpty)
    .toList

  private def reduce2(tokens: List[String]): Long =
    reduce(reducePlusParens(tokens))

  private def reduce(tokens: List[String]): Long = tokens match {
    case num :: Nil => num.toLong
    case a :: op :: "(" :: tail =>
      // find matching closing paren
      val matchingEnd = matchingClosingParen(tail)
      val sub = tail.slice(0, matchingEnd)
      val rest = tail.slice(matchingEnd + 1, tail.length)
      // and reduce sub expression, and then reduce the rest
      reduce(a :: op :: reduce(sub).toString :: rest)
    case a :: "+" :: b :: tail if a.toLongOption.isDefined && b.toLongOption.isDefined =>
      reduce((a.toLong + b.toLong).toString :: tail)
    case a :: "*" :: b :: tail if a.toLongOption.isDefined && b.toLongOption.isDefined =>
      reduce((a.toLong * b.toLong).toString :: tail)
    case otherwise =>
      reduce("1" :: "*" :: otherwise)
  }

  // Only reduce parens and pluses, muls have lowest precedence
  private def reducePlusParens(tokens: List[String]): List[String] = tokens match {
    case Nil => Nil
    case num :: Nil => num :: Nil
    case a :: op :: "(" :: tail =>
      // find matching closing paren
      val matchingEnd = matchingClosingParen(tail)
      val sub = tail.slice(0, matchingEnd)
      val rest = tail.slice(matchingEnd + 1, tail.length)
      // and reduce sub expression, and then reduce the rest
      reducePlusParens(a :: op :: reduce(reducePlusParens(sub)).toString :: rest)
    case a :: "+" :: b :: tail if a.toLongOption.isDefined && b.toLongOption.isDefined =>
      reducePlusParens((a.toLong + b.toLong).toString :: tail)
    case a :: "*" :: b :: tail if a.toLongOption.isDefined && b.toLongOption.isDefined =>
      a :: "*" :: reducePlusParens(b :: tail)
    case otherwise =>
      reducePlusParens("1" :: "*" :: otherwise)
  }

  private def matchingClosingParen(tokens: List[String]): Int = {
    var idx = 0
    var depth = 1
    while(idx < tokens.length) {
      if(tokens(idx) == "(") {
        depth += 1
      } else if(tokens(idx) == ")") {
        depth -= 1
      }
      if(depth == 0) {
        return idx
      }
      idx += 1
    }
    ???
  }
}

package nl.kevinvandervlist.aoc2015.day05

object NaughtyNice {
  def one(in: List[String]): Int = in
    .map(StrOne.apply)
    .count(_.isNice)

  def two(in: List[String]): Int = in
    .map(StrTwo.apply)
    .count(_.isNice)
}

private case class StrOne(input: String) {
  def isNice: Boolean = noForbiddenStrings && atLeastThreeVowels && atLeastOneLetterTwiceInARow

  def vowels: List[Char] = List(
    'a',
    'e',
    'o',
    'u',
    'i'
  )

  def atLeastThreeVowels: Boolean = input
    .toCharArray
    .partition(vowels.contains)
    ._1.length >= 3

  def atLeastOneLetterTwiceInARow: Boolean = {
    val chars = input.toCharArray
    val rest = chars.tail
    rest.foldLeft(false -> chars.head) {
      case (s, _) if s._1 => s
      case (s, char) if s._2 == char => (true, char)
      case (_, char) => (false, char)
    }._1
  }

  def forbidden: List[String] = List(
    "ab",
    "cd",
    "pq",
    "xy"
  )

  def noForbiddenStrings: Boolean = ! forbidden.exists(input.contains)
}

private case class StrTwo(input: String) {
  def isNice: Boolean = nonOverlappingPair && intermittentRepetition

  def nonOverlappingPair: Boolean = {
    val maybePairs = input
      .toCharArray
      .sliding(2)
      .toList
      .map(_.mkString(""))
      .groupBy(identity)
      .filter(_._2.size > 1)
      .values
      .flatten
      .toList
      .distinct

    var idx = 0
    while(idx < maybePairs.size) {
      if(input.drop(input.indexOf(maybePairs(idx)) + 2).indexOf(maybePairs(idx)) >= 0) {
        return true
      }
      idx += 1
    }
    false
  }

  def intermittentRepetition: Boolean = input
    .toCharArray
    .sliding(3)
    .exists(x => x(0) == x(2))
}

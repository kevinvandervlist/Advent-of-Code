package nl.kevinvandervlist.aoc2020.day02

object PasswordPolicy {

  def validPasswordsOne(in: List[String]): Int = in
    .map(passwordEntry)
    .count(_.isValidOne)

  def validPasswordsTwo(in: List[String]): Int = in
    .map(passwordEntry)
    .count(_.isValidTwo)

  private def passwordEntry(line: String): PasswordEntry = {
    val splitted = line.split(Array(' ', '-', ':'))
    PasswordEntry(
      splitted(0).toInt,
      splitted(1).toInt,
      splitted(2).head,
      splitted(4),
      line
    )
  }
}

private case class PasswordEntry(min: Int, max: Int, letter: Char, entry: String, raw: String) {
  def isValidOne: Boolean = {
    val count = entry.count(_ == letter)
    val lower = count >= min
    val upper = count <= max
    lower && upper
  }

  def isValidTwo: Boolean = {
    val first = entry(min - 1) == letter
    val second = entry(max - 1) == letter
    (first != second) && (first || second)
  }
}
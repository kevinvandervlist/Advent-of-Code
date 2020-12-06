package nl.kevinvandervlist.aoc2020.day06

object CustomsDeclaration {
  def one(in: String): Int =
    separateGroups(in).map(_.anyYes.size).sum
  def two(in: String): Int =
    separateGroups(in).map(_.allYes.size).sum

  private def separateGroups(batch: String): List[Group] =
    batch.split("\n\n").map(Group.apply).toList
}

case class Group(individuals: List[List[Char]]) {
  def anyYes: List[Char] = individuals.flatten.distinct
  def allYes: List[Char] = individuals.tail.map(_.toSet).foldLeft(individuals.head.toSet) {
    case (intersection, nxt) => intersection.intersect(nxt)
  }.toList
}

object Group {
  def apply(individualBlock: String): Group = Group(
    individualBlock
      .split('\n')
      .toList
      .map(answers => answers.toCharArray.toList)
  )
}

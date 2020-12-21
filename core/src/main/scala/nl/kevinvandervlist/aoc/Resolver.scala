package nl.kevinvandervlist.aoc

object Resolver {
  /**
   * Solver for a simple constraint problem
   *
   * Given a map of (k -> { opts }), which outcomes exist such that for every o in (k -> o), o is unique.
   */
  def solve[K, V](maybe: Map[K, Iterable[V]]): List[Map[K, V]] =
    solve(Map.empty, maybe)

  // Todo: tail recursion
  private def solve[K, V](chosen: Map[K, V], maybe: Map[K, Iterable[V]]): List[Map[K, V]] = {
    if (maybe.isEmpty) {
      return List(chosen)
    }
    // Find the choice with least amount of options to limit recursion complexity
    val choice = maybe.keys.minBy(k => maybe(k).size)
    val remainder = maybe - choice
    maybe(choice).iterator.flatMap(r => {
      val updatedRemainder = remainder.map {
        case (x, remainingRules) => x -> remainingRules.filterNot(_ == r)
      }
      // There remains a field that does not have valid options anymore
      if(updatedRemainder.exists(_._2.isEmpty)) {
        return List.empty
      } else {
        solve(chosen + (choice -> r), updatedRemainder)
      }
    }).filterNot(_.isEmpty).toList
  }
}

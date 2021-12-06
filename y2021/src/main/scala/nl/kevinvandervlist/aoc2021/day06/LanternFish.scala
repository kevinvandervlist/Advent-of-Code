package nl.kevinvandervlist.aoc2021.day06

object LanternFish {
  def one(in: String): Long =
    calculate(80, in.split(',').map(_.toInt))

  def two(in: String): Long =
    calculate(256, in.split(',').map(_.toInt))

  def calculate(days: Int, seed: Iterable[Int]): Long = {
    val initialState: Map[Int, Long] = seed.foldLeft(Map.empty[Int, Long]) {
      case (map, day) => map + (day -> (map.getOrElse(day, 0L) + 1))
    }
    val result = (1 to days).foldLeft(initialState) {
      case (oldState, _) =>
        val newStateKeys = oldState.keys.toList.map {
          case 0 => 6 -> oldState.getOrElse(0, 0L)
          case d => (d - 1) -> oldState.getOrElse(d, 0L)
        } :+ 8 -> oldState.getOrElse(0, 0L)
        val newState = newStateKeys.foldLeft(Map.empty[Int, Long]) {
          case (m, (k, v)) => m + (k -> (m.getOrElse(k, 0L) + v))
        }
        newState
    }
    result.values.sum
  }
}

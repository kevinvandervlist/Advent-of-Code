package nl.kevinvandervlist.aoc2021.day05

object HydrothermalVenture {
  def one(in: List[String]): Int = countAtleast2(
    in.map(parseCoords)
      .filterNot(c => isDiagonal(c._1, c._2))
      .map(c => expand(c._1, c._2))
      .flatten
  )
  def two(in: List[String]): Int = countAtleast2(
    in.map(parseCoords)
      .map(c => expand(c._1, c._2))
      .flatten
  )

  private def countAtleast2(coords: List[(Int, Int)]): Int = {
    coords.foldLeft(Map.empty[(Int, Int), Int]) {
      case (map, coord) => map + (coord -> (map.get(coord).getOrElse(0) + 1))
    }.count(_._2 > 1)
  }

  private def parseCoords(line: String): ((Int, Int), (Int, Int)) = {
    val splitted = line.split(" -> ")
    val fst = splitted(0).split(',')
    val snd = splitted(1).split(',')
    (fst(0).toInt -> fst(1).toInt) -> (snd(0).toInt -> snd(1).toInt)
  }

  private def expand(start: (Int, Int), end: (Int, Int)): List[(Int, Int)] = {
    if(start._1 == end._1) {
      // vertical
      val s = math.min(start._2, end._2)
      val e = math.max(start._2, end._2)
      (s to e).toList.map(c => start._1 -> c)
    } else if(start._2 == end._2) {
      // horizontal
      val s = math.min(start._1, end._1)
      val e = math.max(start._1, end._1)
      (s to e).toList.map(c => c -> start._2)
    } else {
      var xs = (math.min(start._1, end._1) to math.max(start._1, end._1)).toList
      var ys = (math.min(start._2, end._2) to math.max(start._2, end._2)).toList
      if(start._1 > end._1) {
        xs = xs.reverse
      }
      if(start._2 > end._2) {
        ys = ys.reverse
      }
      xs.zip(ys)
    }
  }

  private def isDiagonal(start: (Int, Int), end: (Int, Int)): Boolean =
    (start._1 != end._1) && (start._2 != end._2)
}

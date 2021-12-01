package nl.kevinvandervlist.aoc2021.day01

object SonarSweep {
  val INC = "(increased)"
  val DEC = "(decreased)"

  def one(ints: List[String]): Int = {
    measurements(ints.map(_.toInt)).count(_._2 == INC)
  }

  def two(ints: List[String]): Int = {
    val ns = ints.map(_.toInt)
    val byThree = ns.zip(ns.tail).zip(ns.tail.tail).map {
      case ((a,b), c) => a + b + c
    }
    measurements(byThree).count(_._2 == INC)
  }

  private def measurements(in: List[Int]): List[(Int, String)] = {
    val rest: List[(Int, String)] = in.tail.zip(in).map {
      case (a, b) if a > b => a -> INC
      case (a, _) => a -> DEC
    }
    (in.head -> "(N/A - no previous measurement)") :: rest
  }
}

package nl.kevinvandervlist.aoc2021.day08

object SevenSegmentSearch {
  private val nums: Map[Int, Int] = Map(
    2 -> 1,
    4 -> 4,
    3 -> 7,
    7 -> 8
  )

  def one(input: List[String]): Int = {
    val ends = input.map(_.split('|')(1).trim)
    val possible = ends.flatMap(_.split(' ').map(_.length))
    possible.count(nums.keySet.contains)
  }

  def two(input: List[String]): Int =
    input.map(decode).sum

  def decode(line: String): Int = {
    val toDecode: Set[Set[Char]] = line.split('|')(0).trim.split(' ').map(_.toSet).toSet
    val toGuess: Array[Set[Char]] = line.split('|')(1).trim.split(' ').map(_.toSet)
    val decMap: Map[Set[Char], Int] = decoder(toDecode)
    s"${decMap(toGuess(0))}${decMap(toGuess(1))}${decMap(toGuess(2))}${decMap(toGuess(3))}".toInt
  }

  private def decoder(toDecode: Set[Set[Char]]): Map[Set[Char], Int] = {
    val one = {
      val x = toDecode.filter(_.size == 2)
      assert(x.sizeIs == 1)
      x.head
    }
    val four = {
      val x = toDecode.filter(_.size == 4)
      assert(x.sizeIs == 1)
      x.head
    }
    val seven = {
      val x = toDecode.filter(_.size == 3)
      assert(x.sizeIs == 1)
      x.head
    }
    val eight = {
      val x = toDecode.filter(_.size == 7)
      assert(x.sizeIs == 1)
      x.head
    }
    val three = {
      // 3 is 5 elements, should intersect with '1' because of the right bar
      val x = toDecode.filter(_.size == 5).filter(s => s.intersect(one) == one)
      assert(x.sizeIs == 1)
      x.head
    }
    // The diff between 4 and 1
    val angle_bracket = four.diff(one)
    val five = {
      // five is 5 elements. Should also intersect with 'angle_bracket'
      val x = toDecode.filter(_.size == 5).filter(s => s.intersect(angle_bracket) == angle_bracket)
      assert(x.sizeIs == 1)
      x.head
    }
    val two = {
      // Two is not 3 and not 5
      val x = toDecode.filter(_.size == 5).filterNot(_ == three).filterNot(_ == five)
      assert(x.sizeIs == 1)
      x.head
    }
    val nine = {
      // 9 is 6 elements, should intersect with '4'
      val x = toDecode.filter(_.size == 6).filter(s => s.intersect(four) == four)
      assert(x.sizeIs == 1)
      x.head
    }

    val six = {
      // 6 is 6 elements, should intersect with angle_bracket and not be nine
      val x = toDecode.filter(_.size == 6)
        .filter(s => s.intersect(angle_bracket) == angle_bracket)
        .filterNot(_ == nine)
      assert(x.sizeIs == 1)
      x.head
    }

    val zero = {
      // Two is not 6 and not 9
      val x = toDecode.filter(_.size == 6).filterNot(_ == six).filterNot(_ == nine)
      assert(x.sizeIs == 1)
      x.head
    }

    Map(
      zero -> 0,
      one -> 1,
      two -> 2,
      three -> 3,
      four -> 4,
      five -> 5,
      six -> 6,
      seven -> 7,
      eight -> 8,
      nine -> 9
    )
  }
}
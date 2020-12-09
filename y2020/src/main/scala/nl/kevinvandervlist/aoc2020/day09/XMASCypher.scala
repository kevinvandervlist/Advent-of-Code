package nl.kevinvandervlist.aoc2020.day09

object XMASCypher {
  def one(in: List[String], circle: Int): Long =
    findWeakness(in.map(_.toLong), circle)

  def two(in: List[String], circle: Int): Long = {
    findWeaknessSegment(in.map(_.toLong), circle)
  }

  private def findWeaknessSegment(data: List[Long], circle: Int): Long = {
    val weakness = findWeakness(data, circle)
    var segmentStart = 0
    var segmentEnd = 0
    var cnt = 0L
    while(segmentStart < data.size) {
      segmentEnd = segmentStart + 1
      cnt = data(segmentStart)
      while(segmentEnd < data.size) {
        cnt += data(segmentEnd)
        if(cnt == weakness) {
          val range = data.slice(segmentStart, segmentEnd + 1)
          return range.min + range.max
        } else if(cnt > weakness) {
          segmentEnd = data.size
        } else {
          segmentEnd += 1
        }
      }
      segmentStart += 1
    }

    -1
  }

  private def findWeakness(data: List[Long], circle: Int): Long = {
    var cb = emptyCircularBuffer[Long]
    val preamble = data.take(circle)
    val rest = data.drop(circle)

    val add = addToCircularBuffer[Long](circle)(_, _)
    for(p <- preamble) {
      cb = add(cb, p)
    }
    LazyList.from(rest).map {
      case n if ! containsSum(cb, n) => Some(n)
      case n => cb = add(cb, n); None
    }.collectFirst {
      case Some(x) => x
    }.get
  }

  private def containsSum(numbers: Vector[Long], goal: Long): Boolean = numbers
    .combinations(2)
    .collectFirst {
      case parts if parts.sum == goal => true
    }.getOrElse(false)

  private type CircularBuffer[T] = Vector[T]

  private def emptyCircularBuffer[T] : CircularBuffer[T] = Vector.empty[T]

  private def addToCircularBuffer[T](maxSize : Int)(buffer : CircularBuffer[T], item : T) : CircularBuffer[T]  =
    (buffer :+ item) takeRight maxSize
}

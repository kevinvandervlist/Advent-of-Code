package nl.kevinvandervlist.aoc2020.day13

object ShuttleSearch {
  def one(in: List[String]): Int = {
    val earliest = in.head.toInt
    val schedule = parseSchedule(in(1))
    val (bus, waitingTime) = schedule.earliestBusWithWaitingTime(earliest)
    bus * waitingTime
  }

  def two(start: Long, in: List[String]): BigInt =
    parseSchedule2(in(1)).copy(start = start).bigJump

  private def parseSchedule(line: String): Schedule = Schedule(line
    .split(Array('x', ','))
    .filterNot(_.isEmpty)
    .map(_.toInt)
    .sorted
    .toList
  )

  private def parseSchedule2(line: String): Schedule2 = Schedule2(0, line
    .split(Array(','))
    .filterNot(_.isEmpty)
    .map(_.toIntOption)
    .toVector
  )
}

private case class Schedule(interval: List[Int]) {
  def earliestBusWithWaitingTime(start: Int): (Int, Int) = {
    var busNo = Integer.MAX_VALUE
    var waitingTime = Integer.MAX_VALUE
    var idx = 0
    while(idx < interval.size) {
      val x = start % interval(idx)
      val newWaitingTime = (interval(idx) - x)
      if(waitingTime > newWaitingTime) {
        waitingTime = newWaitingTime
        busNo = interval(idx)
      }
      idx += 1
    }
    (busNo, waitingTime)
  }
}

private case class Schedule2(start: Long, interval: Vector[Option[Int]]) {
  def bigJump: BigInt = {
    var idx = start
    var busCnt = 0
    var _loopBusCnt = 0
    var step = 1L
    var bdx = 0
    while(true) {
      assert(idx <= 539746751134958L, s"IDX $idx > expected result (step: $step)")
      while(bdx < interval.length) {
        interval(bdx) match {
          case None => bdx += 1
          case Some(b) if ((idx + bdx) % b) == 0 =>
            _loopBusCnt += 1
            if(_loopBusCnt > busCnt) {
              if(busCnt == 1) {
                step = b
              } else {
                step *= b
              }
              busCnt += 1
            }
            bdx += 1
          case Some(_) => bdx = Integer.MAX_VALUE
        }
      }
      if(bdx < Integer.MAX_VALUE) {
        return idx
      }
      bdx = 0
      _loopBusCnt = 0
      idx += step
    }
    ???
  }
}
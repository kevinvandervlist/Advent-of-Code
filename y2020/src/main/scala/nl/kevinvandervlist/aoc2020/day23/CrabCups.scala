package nl.kevinvandervlist.aoc2020.day23

object CrabCups {
  def one(in: String): String = {
    val state = in.toCharArray.map(_.toString.toInt)
    val game = Cups(state.toVector)
    val endState = (0 until 100).foldLeft(game) {
      case (g, _) => g.nextRound()
    }
    endState.answer
  }

  def two(in: String): String = {
    val state = in.toCharArray.map(_.toString.toInt) ++ (10 to 1_000_000)
    val game = new Cups2(state.toVector, state.max)
    val endState = (0 until 10_000_000).foldLeft(game) {
      case (g, _) => g.nextRound()
    }
    endState.answer
  }
}

private case class Cups(values: Vector[Int]) {
  private def nextRotation(values: Vector[Int], numberOfSteps: Int): Vector[Int] =
    values.drop(numberOfSteps) ++ values.take(numberOfSteps)

  private def nextLabel(removed: Vector[Int], currentValue: Int): Int = {
    val range = (1 until currentValue).reverse ++ (currentValue to values.length).reverse
    range
      .find(v => ! (removed contains v))
      .get
  }

  private def doTurnShuffle(): Vector[Int] = {
    val removed = values.slice(1, 4)
    val remaining = values.head +: values.slice(4, values.size)
    val destinationIndex = remaining.indexOf(nextLabel(removed, values(0))) + 1
    remaining.take(destinationIndex) ++ removed ++ remaining.drop(destinationIndex)
  }

  def answer: String = {
    val fstPos = values.indexOf(1)
    val rotated = nextRotation(values, fstPos)
    rotated.tail.mkString
  }

  def nextRound(): Cups =
    Cups(nextRotation(doTurnShuffle(), 1))
}

private class Cups2(values: Vector[Int], max: Int) {
  private var cur: SLLE = new SLLE(values.head)
  private var dest: SLLE = new SLLE(values.head)
  private val last = values.tail.foldLeft(cur) {
    case (acc, l) => acc.setNext(new SLLE(l))
  }
  last.setNext(cur)
  val lookup = createLookupTable(cur, last, values.length)

  private def createLookupTable(first: SLLE, last: SLLE, size: Int): Array[SLLE] = {
    val result = new Array[SLLE](size + 1)
    var c = first
    while(c.next != first) {
      result(c.label) = c
      c = c.next
    }
    result(last.label) = c
    result
  }

  private def nextSelected(cur: SLLE, a: Int, b: Int, c: Int): SLLE = {
    var l = cur.label - 1
    while (true) {
      if (l <= 0) {
        l = max;
      }
      if (l != a && l != b && l != c) {
        return lookup(l)
      }
      l -= 1
    }
    ???
  }

  private def doTurnShuffle(): Unit = {
    // take three and remove them, while setting next/dest properly
    val one = cur.next
    val two = cur.next.next
    val three = cur.next.next.next
    dest = nextSelected(cur, one.label, two.label, three.label)
    cur.setNext(three.next)
    cur = three.next
    three.setNext(dest.next)
    dest.setNext(one)
  }

  def answer: String = {
    val c = lookup(1)
    val mul = c.next.label.toLong * c.next.next.label.toLong
    mul.toString
  }

  def nextRound(): Cups2 = {
    doTurnShuffle()
    this
  }
}

private class SLLE(val label: Int) {
  var next: SLLE = _
  def setNext(nxt: SLLE): SLLE = {
    next = nxt
    next
  }

  override def toString: String = label.toString
}

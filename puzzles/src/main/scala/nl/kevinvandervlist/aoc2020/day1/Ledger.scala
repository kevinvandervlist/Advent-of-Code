package nl.kevinvandervlist.aoc2020.day1

import scala.collection.mutable.ListBuffer

object Ledger {
  val goal = BigInt(2020)

  def fromStringsTwo(in: List[String]): BigInt =
    two(in.map(BigInt.apply))

  def fromStringsThree(in: List[String]): BigInt =
    three(in.map(BigInt.apply))

  def two(in: List[BigInt]): BigInt = {
    var a = 0
    var b = 1
    val parts = new ListBuffer[BigInt]
    while(a < in.size) {
      b = a + 1
      while(b < in.size) {
        if((in(a) + in(b)) == goal) {
          parts.addOne(in(a))
          parts.addOne(in(b))
        }
        b += 1
      }
      a += 1
    }
    parts.product
  }

  def three(in: List[BigInt]): BigInt = {
    var a = 0
    var b = 1
    var c = 2
    val parts = new ListBuffer[BigInt]
    while(a < in.size) {
      b = a + 1
      while(b < in.size) {
        c = b + 1
        while(c < in.size) {
          if((in(a) + in(b) + in(c)) == goal) {
            parts.addOne(in(a))
            parts.addOne(in(b))
            parts.addOne(in(c))
          }
          c += 1
        }
        b += 1
      }
      a += 1
    }
    parts.product
  }
}

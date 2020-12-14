package nl.kevinvandervlist.aoc2020.day14

import scala.annotation.tailrec
import scala.collection.mutable

object DockingData {
  def one(in: List[String]): Long = {
    val state = new mutable.HashMap[Long, Long]()
    var mask = parseMask(in.head)
    parseAssignment(in.tail.head)
    in.tail.foreach( i => {
      if(i.startsWith("mem")) {
        val (idx, value) = parseAssignment(i)
        state += (idx -> applyMask(value, mask))
      } else {
        mask = parseMask(i)
      }
    })
    state.values.sum
  }

  def two(in: List[String]): Long = {
    val state = new mutable.HashMap[Long, Long]()
    var mask = parseMask(in.head)
    parseAssignment(in.tail.head)
    in.tail.foreach( i => {
      if(i.startsWith("mem")) {
        val (mem, value) = parseAssignment(i)
        val idxes = applyMask2(mem, mask)
        for(idx <- idxes) {
          state += (idx -> value)
        }
      } else {
        mask = parseMask(i)
      }
    })
    state.values.sum
  }

  @inline
  private def thirtySixZeroes = "000000000000000000000000000000000000"

  @inline
  private def parseMask(raw: String): String = raw
    .split(Array(' ', '='))
    .last

  @inline
  private def parseAssignment(raw: String): (Long, Long) = {
    val splitted = raw.split(Array(' ', '=', '[', ']'))
    splitted(1).toLong -> splitted.last.toLong
  }

  private def applyMask(value: Long, mask: String): Long = {
    val result = toBinaryString(value).zip(mask).map {
      case (v, m) => m match {
        case 'X' => v
        case '1' => '1'
        case '0' => '0'
      }
    }
    toLong(result.mkString(""))
  }

  private def applyMask2(l: Long, mask: String): Vector[Long] = {
    @tailrec
    def masks(resultMasks: Vector[Vector[Char]], value: Vector[Char], mask: Vector[Char]): Vector[Vector[Char]] = mask match {
      case head +: tail => head match {
        case '0' =>
          masks(resultMasks.map(_.appended(value.head)), value.tail, tail)
        case '1' =>
          masks(resultMasks.map(_.appended('1')), value.tail, tail)
        case 'X' =>
          val zeroes = resultMasks.map(_.appended('0'))
          val ones = resultMasks.map(_.appended('1'))
          masks(zeroes ++ ones, value.tail, tail)
      }
      case _ => resultMasks
    }

    masks(Vector(Vector.empty), toBinaryString(l).toVector, mask.toVector)
      .map(_.mkString(""))
      .map(toLong)
  }

  @inline
  private def toBinaryString(value: Long): String =
    (thirtySixZeroes + value.toBinaryString).takeRight(thirtySixZeroes.length)

  @inline
  private def toLong(value: String): Long =
    BigInt.apply(value, 2).toLong
}
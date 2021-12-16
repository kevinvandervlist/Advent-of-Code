package nl.kevinvandervlist.aoc2021.day16

import nl.kevinvandervlist.aoc.Binary

import scala.language.implicitConversions

object PacketDecoder {
  // I should really invest in a hand rolled parser combinator next year...
  def one(in: String): Int =
    sum(parse(in))

  def two(in: String): Long =
    interpret(parse(in))

  private def sum(p: Packet): Int = p match {
    case Literal(v, _, _) => v
    case OperatorPacket(v, _, members) => v + members.map(sum).sum
  }

  private def toHexString(in: String): String =
    in.map(Binary.parseHex).map(Binary.toBinary(_, 4)).mkString("")

  private def parse(in: String): Packet =
    parsePacket(toHexString(in))._1

  private implicit def asInt(b: Boolean): Long = if(b) 1 else 0

  private def interpret(p: Packet): Long = p match {
    case Literal(_, _, num) => num
    case OperatorPacket(_, 0, members) => members.map(interpret).sum
    case OperatorPacket(_, 1, members) => members.map(interpret).product
    case OperatorPacket(_, 2, members) => members.map(interpret).min
    case OperatorPacket(_, 3, members) => members.map(interpret).max
    case OperatorPacket(_, 5, a :: b :: _) => interpret(a) > interpret(b)
    case OperatorPacket(_, 6, a :: b :: _) => interpret(a) < interpret(b)
    case OperatorPacket(_, 7, a :: b :: _) => interpret(a) == interpret(b)
  }

  private def parsePacket(hex: String): (Packet, String) = {
    val version = Binary.parseBinary(hex.substring(0, 3)).toInt
    val typeid = Binary.parseBinary(hex.substring(3, 6)).toInt
    if(typeid == 4) {
      // parse lit
      val (lit, rest) = parseLiteral(hex.substring(6))
      Literal(version, typeid, lit) -> rest
    } else {
      // parse operator
      val (mem, rest) = parseOperator(hex.substring(6))
      OperatorPacket(version, typeid, mem) -> rest
    }
  }

  private def parseOperator(hex: String): (List[Packet], String) = {
    val lengthTypeID = hex.substring(0, 1)
    var packets = List.empty[Packet]
    var remainder: String = ""

    lengthTypeID match {
      case "0" =>
        // next 15 bits are a number that represents the total length in bits of the sub-packets
        // contained by this packet
        val p = 16
        val len = Binary.parseBinary(hex.substring(1, p)).toInt
        var subs = hex.substring(p, p + len)
        remainder = hex.substring(p + len)
        while(subs.nonEmpty) {
          val(packet, rem) = parsePacket(subs)
          subs = rem
          packets = packet :: packets
        }
      case "1" =>
        // next 11 bits are a number that represents the number of sub-packets immediately
        // contained by this packet.
        val p = 12
        var num = Binary.parseBinary(hex.substring(1, p)).toInt
        remainder = hex.substring(p)
        while(num > 0) {
          val (packet, rem) = parsePacket(remainder)
          packets = packet :: packets
          remainder = rem
          num -= 1
        }
    }
    packets.reverse -> remainder
  }

  private def parseLiteral(in: String): (Long, String) = {
    var p = 0
    var parts = List.empty[String]
    while(p + 5 <= in.length) {
      val x = in.substring(p, p + 5)
      x.head match {
        case '1' => parts = x.tail :: parts
        case '0' => return Binary.parseBinary((x.tail :: parts).reverse.mkString("")) -> in.substring(p + 5)
      }
      p += 5
    }
    throw new IllegalStateException("Cannot be reached")
  }
}

private sealed trait Packet {
  def version: Int
  def typeid: Int
}
private case class OperatorPacket(version: Int, typeid: Int, members: List[Packet]) extends Packet
private case class Literal(version: Int, typeid: Int, num: Long) extends Packet
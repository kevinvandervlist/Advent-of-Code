package nl.kevinvandervlist.aoc

object Binary {
  def parseHex(hex: Char): Long =
    parseHex(hex.toString).toLong

  def parseHex(hex: String): Long =
    Integer.parseInt(hex, 16)

  def parseBinary(bin: String): Long =
    java.lang.Long.parseLong(bin, 2)

  def toBinary(n: Long, lengthIncludingPadding: Int): String =
    String.format (s"%0${lengthIncludingPadding}d", n.toBinaryString.toLong)
}

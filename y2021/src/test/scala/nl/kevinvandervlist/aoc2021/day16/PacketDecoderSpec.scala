package nl.kevinvandervlist.aoc2021.day16

import nl.kevinvandervlist.aoc.AoCSpec

class PacketDecoderSpec extends AoCSpec {
  override def example: String = ""

  override def examplePartOne(): Any = {
    PacketDecoder.one("8A004A801A8002F478") shouldBe 16
    PacketDecoder.one("620080001611562C8802118E34") shouldBe 12
    PacketDecoder.one("C0015000016115A2E0802F182340") shouldBe 23
    PacketDecoder.one("A0016C880162017C3686B18A3D4780") shouldBe 31
  }

  override def assignmentPartOne(): Any =
    PacketDecoder.one(inputAsString) shouldBe 913

  override def examplePartTwo(): Any = {
    PacketDecoder.two("C200B40A82") shouldBe 3
    PacketDecoder.two("04005AC33890") shouldBe 54
    PacketDecoder.two("880086C3E88112") shouldBe 7
    PacketDecoder.two("CE00C43D881120") shouldBe 9
    PacketDecoder.two("D8005AC2A8F0") shouldBe 1
    PacketDecoder.two("F600BC2D8F") shouldBe 0
    PacketDecoder.two("9C005AC2F8F0") shouldBe 0
    PacketDecoder.two("9C0141080250320F1802104A08") shouldBe 1
  }

  override def assignmentPartTwo(): Any =
    PacketDecoder.two(inputAsString) shouldBe 1510977819698L
}
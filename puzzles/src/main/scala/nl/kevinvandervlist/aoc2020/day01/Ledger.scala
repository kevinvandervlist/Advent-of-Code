package nl.kevinvandervlist.aoc2020.day01

object Ledger {
  val goal = BigInt(2020)

  def fromStringsTwo(in: List[String]): BigInt =
    productByParts(in.map(BigInt.apply), 2)

  def fromStringsThree(in: List[String]): BigInt =
    productByParts(in.map(BigInt.apply), 3)

  def productByParts(in: List[BigInt], parts: Int): BigInt = in
    .combinations(parts)
    .collect {
      case parts if parts.sum == goal => parts
    }.flatten
    .product
}

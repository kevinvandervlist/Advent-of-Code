package nl.kevinvandervlist.aoc2020.day22

import scala.collection.mutable

object CrabCombat {
  def one(in: List[String]): Int = {
    val (p1Deck, p2Deck) = parse(in)
    val p1 = new mutable.Queue[Int]()
    p1.addAll(p1Deck)
    val p2 = new mutable.Queue[Int]()
    p2.addAll(p2Deck)
    while(p1.nonEmpty && p2.nonEmpty) {
      val c1 = p1.dequeue()
      val c2 = p2.dequeue()
      if(c1 > c2) {
        p1.enqueue(c1, c2)
      } else {
        p2.enqueue(c2, c1)
      }
    }
    gameValue(p1, p2)
  }

  def two(in: List[String]): Int = {
    val (p1Deck, p2Deck) = parse(in)
    val (_, deck) = playRec(Set.empty, p1Deck, p2Deck)
    gameValue(deck, List.empty)
  }

  private def gameValue(p1: Iterable[Int], p2: Iterable[Int]): Int = (p1.toList ++ p2.toList)
    .reverseIterator
    .zipWithIndex
    .map {
      case (card, idx) => card * (idx + 1)
    }.sum

  @inline
  private def canRecurse(cards1: List[Int], cards2: List[Int]): Boolean =
    cards1.head <= cards1.tail.size && cards2.head <= cards2.tail.size

  def playRec(seen: Set[Int], p1: List[Int], p2: List[Int]): (Boolean, List[Int]) = {
    if (p1.isEmpty) {
      return (false, p2)
    }
    if (p2.isEmpty) {
      return (true, p1)
    }

    // Assume we don't get a hash collision. -1 separates the decks. Alternative is take the whole thing as a string
    val key = (p1 ++ (-1 :: p2)).hashCode()
    if (seen.contains(key)) {
      return (true, p1) // p1 wins
    }
    val updatedSeen = seen + key

    var p1won = false
    if (canRecurse(p1, p2)) {
      p1won = playRec(updatedSeen, p1.slice(1, p1.head + 1), p2.slice(1, p2.head + 1))._1
    } else {
      p1won = p1.head > p2.head
    }

    if (p1won) {
      playRec(updatedSeen, p1.tail ++ List(p1.head, p2.head), p2.tail)
    } else {
      playRec(updatedSeen, p1.tail, p2.tail ++ List(p2.head, p1.head))
    }
  }

  private def parse(in: List[String]): (List[Int], List[Int]) = {
    val p1 = in.takeWhile(_ != "Player 2:").drop(1).dropRight(1)
    val p2 = in.drop(p1.length + 3)
    p1.map(_.toInt) -> p2.map(_.toInt)
  }
}
package nl.kevinvandervlist.aoc2021.day23

import nl.kevinvandervlist.aoc.Characters
import nl.kevinvandervlist.aoc2021.day23.GameState.{costOfMove, ownsRoom, targetRoom}

import scala.collection.mutable

object Amphipod {
  def one(in: (Int, List[Char], List[Char], List[Char], List[Char])): Long = {
    val initial = GameState(in)

    val pq = mutable.PriorityQueue.empty[GameState](
      Ordering.by((_: GameState).totalCost).reverse
    )
    val seen = mutable.Map[GameState, Long](initial -> initial.totalCost)
    pq.addOne(initial)
    while (true) {
      val current = pq.dequeue()
//      println(s"current: ${current.totalCost}; remaining: ${pq.size}")
      if (current.hasWon) {
        val game = current.steps.reverse.zipWithIndex
        for((g, gidx) <- game) {
          println(s"Game: $gidx, total cost ${g.totalCost}")
          println(g.print)
        }
        return current.totalCost
      }
      val nextMoves = current.nextStates
      for (n <- nextMoves) {
        val nilCost = n.copy(totalCost = 0, prev = None)
        if((! seen.contains(nilCost)) || n.totalCost < seen(nilCost)) {
          seen.addOne(nilCost -> n.totalCost)
          pq.addOne(n)
        }
      }
    }
    ???
  }
}

object GameState {
  def apply(in: (Int, List[Char], List[Char], List[Char], List[Char])): GameState = {
    GameState(
      0,
      Vector.fill(in._1 - 4 /* reserved spots at room entrance */)(None),
      Vector.from(List(
        Vector.from(in._2.map(x => Some(x))),
        Vector.from(in._3.map(x => Some(x))),
        Vector.from(in._4.map(x => Some(x))),
        Vector.from(in._5.map(x => Some(x))),
      )),
      None
    )
  }

  val costOfMove = Map('A' -> 1, 'B' -> 10, 'C' -> 100, 'D' -> 1000)
  val ownsRoom = Map(0 -> 'A', 1 -> 'B', 2 -> 'C', 3 -> 'D')
  val targetRoom = Map('A' -> 0, 'B' -> 1, 'C' -> 2, 'D' -> 3)
  val stepCost = Map(
    0 -> Map(0 -> 3,1 -> 5,2 -> 7,3 -> 9),
    1 -> Map(0 -> 2,1 -> 4,2 -> 6,3 -> 8),
    2 -> Map(0 -> 2,1 -> 2,2 -> 4,3 -> 6),
    3 -> Map(0 -> 4,1 -> 2,2 -> 2,3 -> 4),
    4 -> Map(0 -> 6,1 -> 4,2 -> 2,3 -> 2),
    5 -> Map(0 -> 8,1 -> 6,2 -> 4,3 -> 2),
    6 -> Map(0 -> 9,1 -> 7,2 -> 5,3 -> 3),
  )
}

case class GameState(totalCost: Long, corridor: Vector[Option[Char]], rooms: Vector[Vector[Option[Char]]], prev: Option[GameState]) {
  def print: String = {
    val _corridor = List(
      corridor.take(2).map(_.getOrElse(Characters.EMPTY).toString).mkString(""),
      Characters.EMPTY.toString,
      corridor.drop(2).take(1).map(_.getOrElse(Characters.EMPTY).toString).mkString(""),
      Characters.EMPTY.toString,
      corridor.drop(3).take(1).map(_.getOrElse(Characters.EMPTY).toString).mkString(""),
      Characters.EMPTY.toString,
      corridor.drop(4).take(1).map(_.getOrElse(Characters.EMPTY).toString).mkString(""),
      Characters.EMPTY.toString,
      corridor.drop(5).take(1).map(_.getOrElse(Characters.EMPTY).toString).mkString(""),
      corridor.drop(6).map(_.getOrElse(Characters.EMPTY).toString).mkString(""),
    ).mkString("")
    val head = List(
      Characters.SQUARE.toString.repeat(corridor.length + 2 + 4),
      Characters.SQUARE.toString + _corridor + Characters.SQUARE.toString,
      Characters.SQUARE.toString.repeat(3) + rooms.map(_.head.getOrElse(Characters.EMPTY).toString).mkString(Characters.SQUARE.toString) + Characters.SQUARE.toString.repeat(3)
    )

    val mid = if(rooms.head.size > 2) {
      List(
        Characters.EMPTY.toString.repeat(2) + Characters.SQUARE.toString + rooms.map(_(rooms.head.size - 3).getOrElse(Characters.EMPTY).toString).mkString(Characters.SQUARE.toString) + Characters.SQUARE.toString + Characters.EMPTY.toString.repeat(2),
        Characters.EMPTY.toString.repeat(2) + Characters.SQUARE.toString + rooms.map(_(rooms.head.size - 2).getOrElse(Characters.EMPTY).toString).mkString(Characters.SQUARE.toString) + Characters.SQUARE.toString + Characters.EMPTY.toString.repeat(2)
      )
    } else {
      List.empty
    }

    val tail = List(
      Characters.EMPTY.toString.repeat(2) + Characters.SQUARE.toString + rooms.map(_(rooms.head.size - 1).getOrElse(Characters.EMPTY).toString).mkString(Characters.SQUARE.toString) + Characters.SQUARE.toString + Characters.EMPTY.toString.repeat(2),
      Characters.EMPTY.toString + Characters.SQUARE.toString.repeat(corridor.length + 4) + Characters.EMPTY.toString,
      s"C: $totalCost"
    )
    List(
      head.mkString("\n"),
      mid.mkString("\n"),
      tail.mkString("\n")
    ).mkString("\n")
  }

  def steps: List[GameState] = prev match {
    case Some(pred) => this :: pred.steps
    case None => List(this)
  }

  def isValid: Boolean = {
    val grouped = (rooms.flatten ++ corridor).groupBy(identity)
    if(grouped(Some('A')).size != rooms.head.size) {
      return false
    }
    if(grouped(Some('B')).size != rooms.head.size) {
      return false
    }
    if(grouped(Some('C')).size != rooms.head.size) {
      return false
    }
    if(grouped(Some('D')).size != rooms.head.size) {
      return false
    }
    true
  }

  def isFinished: Boolean = hasWon

  def hasWon: Boolean = {
    (rooms(0).count(_.getOrElse('X') == ownsRoom(0)) == rooms(0).length) &&
    (rooms(1).count(_.getOrElse('X') == ownsRoom(1)) == rooms(0).length) &&
    (rooms(2).count(_.getOrElse('X') == ownsRoom(2)) == rooms(0).length) &&
    (rooms(3).count(_.getOrElse('X') == ownsRoom(3)) == rooms(0).length)
  }

  private def freeCorridorPositions: Iterable[Int] = corridor.zipWithIndex.collect {
    case (None, pos) => pos
  }

  private def isMoveBlocked(room: Int, roomPosition: Int, corridorPosition: Int): Boolean = {
    // if there is any amphipod in our path, reject this move
    def hasAmphipodAtRoomPosition(roomPosition: Int): Boolean =
      rooms(room)(roomPosition).isDefined
    def hasAmphipodAtCorridorPosition(cPos: Int): Boolean = {
      if(cPos == corridorPosition) {
        return false // explicitly don't care about myself
      }
      corridor(cPos).isDefined
    }

    for(r <- (0 until roomPosition)) {
      if(hasAmphipodAtRoomPosition(r)) {
        return true
      }
    }
    val corridorStart = room match { // should we move right?
      case 0 if corridorPosition >= 2 => room + 2
      case 1 if corridorPosition >= 3 => room + 2
      case 2 if corridorPosition >= 4 => room + 2
      case 3 if corridorPosition >= 5 => room + 2
      case _ => room + 1// no, left
    }
    for(cp <- Math.min(corridorStart, corridorPosition) to Math.max(corridorStart, corridorPosition)) {
      if(hasAmphipodAtCorridorPosition(cp)) {
        return true
      }
    }
    false
  }

  private def makeMove(room: Int, roomPosition: Int, corridorPosition: Int): Option[GameState] = {
    val src = rooms(room)(roomPosition)
    val tgt = corridor(corridorPosition)
    if(src.isDefined || tgt.isDefined) {
      // OK
    } else {
      println("SHOULD NOT HAPPEN")
    }
    val updated= copy(
      this.totalCost + stepsOfMove(room, roomPosition, corridorPosition) * GameState.costOfMove(rooms(room)(roomPosition).getOrElse(corridor(corridorPosition).get)),
      corridor.updated(corridorPosition, rooms(room)(roomPosition)),
      rooms.updated(room, rooms(room).updated(roomPosition, corridor(corridorPosition))),
      prev = Some(this)
    )

    if(! updated.isValid) {
      println("huh")
    }
    if(isMoveBlocked(room, roomPosition, corridorPosition)) {
      None
    } else {
      Some(updated)
    }
  }

  private def stepsOfMove(room: Int, posInRoom: Int, corridorPosition: Int): Int =
    GameState.stepCost(corridorPosition)(room) + posInRoom

  def nextStates: Iterable[GameState] = {
    if(isFinished) {
      Set.empty
    } else {
      val moves = mutable.Set.empty[GameState]
      // move into corridor scenarios
      for((r, ri) <- rooms.zipWithIndex) {
        // move each top node into the corridor, unless all its successors are in their final place.
        for((roomMember, roomPosition) <- r.zipWithIndex) {
          if(roomMember.isDefined && roomMember.get == ownsRoom(ri)) {
            val successorsAreInPlace = roomIsReady(ownsRoom(ri), r.drop(roomPosition + 1))
            if(! successorsAreInPlace) {
              val next = freeCorridorPositions.map(targetPos => makeMove(ri, roomPosition, targetPos))
              next.foreach(_.foreach(moves.addOne))
            }
          } else if(roomMember.isDefined) {
            val next = freeCorridorPositions.map(targetPos => makeMove(ri, roomPosition, targetPos))
            next.foreach(_.foreach(moves.addOne))
          }
        }
      }
      // Move into room scenario
      for((c, ci) <- corridor.zipWithIndex) {
        if(c.isDefined) {
          val r = rooms(targetRoom(c.get))
          // check if there is an empty spot in the correct room
          println(r.zipWithIndex.reverse)
          r.zipWithIndex.reverse.collectFirst {
            case (None, roomPosition) if roomIsReady(c.get, r.drop(roomPosition + 1)) =>
              println(roomPosition)
              println(r.drop(roomPosition + 1))
              makeMove(targetRoom(c.get), roomPosition, ci).foreach(moves.addOne)
            // then check if that empty spot is followed only
          }
        }
      }
      moves
    }
  }

  def roomIsReady(expectedInhabitant: Char, remainder: Vector[Option[Char]]): Boolean = {
    for(r <- remainder) {
      if(r.isEmpty) {
        ??? // should not happen
      }
      if(r.get != expectedInhabitant) {
        return false
      }
    }
    return true
  }
}
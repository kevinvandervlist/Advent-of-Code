package nl.kevinvandervlist.aoc2021.day23

import nl.kevinvandervlist.aoc.Characters
import nl.kevinvandervlist.aoc2021.day23.GameState.{costOfMove, ownsRoom, targetRoom}

import scala.collection.mutable

object Amphipod {
  def challenge(in: (Int, List[Char], List[Char], List[Char], List[Char])): Long = {
    val initial = GameState(in)

    val pq = mutable.PriorityQueue.empty[GameState](
      Ordering.by((_: GameState).totalCost).reverse
    )
    val seen = mutable.Map[GameState, Long](initial -> initial.totalCost)
    pq.addOne(initial)
    while (true) {
      val current = pq.dequeue()
      if (current.hasWon) {
        return current.totalCost
      }
      val nextMoves = current.nextStates

      for (n <- nextMoves) {
        // cost at zero so we can use it as key
        val nilCost = n.copy(totalCost = 0)
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
      Vector.fill(in._1 - 4 /* reserved spots at room entrance, we cannot use them anyway */)(None),
      Vector.from(List(
        Vector.from(in._2.map(x => Some(x))),
        Vector.from(in._3.map(x => Some(x))),
        Vector.from(in._4.map(x => Some(x))),
        Vector.from(in._5.map(x => Some(x))),
      ))
    )
  }

  val costOfMove = Map('A' -> 1, 'B' -> 10, 'C' -> 100, 'D' -> 1000)
  val ownsRoom = Map(0 -> 'A', 1 -> 'B', 2 -> 'C', 3 -> 'D')
  val targetRoom = Map('A' -> 0, 'B' -> 1, 'C' -> 2, 'D' -> 3)
  // pre-calculate the cost for each move, taking into account the cost of the 'forbidden' spot.
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

case class GameState(totalCost: Long, corridor: Vector[Option[Char]], rooms: Vector[Vector[Option[Char]]]) {
  def isValid: Boolean = {
    val grouped = (rooms.flatten ++ corridor).groupBy(identity)

    @inline
    def isNotValidFor(inhabitant: Char): Boolean  =
      grouped(Some(inhabitant)).size != rooms.head.size

    val isAnyNotValid =
      isNotValidFor(('A')) ||
      isNotValidFor(('B')) ||
      isNotValidFor(('C')) ||
      isNotValidFor(('D'))

    ! isAnyNotValid
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
    assert(src.isDefined || tgt.isDefined)

    if(isMoveBlocked(room, roomPosition, corridorPosition)) {
      None
    } else {
      Some(copy(
        this.totalCost + stepsOfMove(room, roomPosition, corridorPosition) * GameState.costOfMove(rooms(room)(roomPosition).getOrElse(corridor(corridorPosition).get)),
        corridor.updated(corridorPosition, rooms(room)(roomPosition)),
        rooms.updated(room, rooms(room).updated(roomPosition, corridor(corridorPosition)))
      ))
    }
  }

  private def stepsOfMove(room: Int, posInRoom: Int, corridorPosition: Int): Int =
    GameState.stepCost(corridorPosition)(room) + posInRoom

  def nextStates: Iterable[GameState] = {
    if(isFinished) {
      Set.empty
    } else {
      val moves = mutable.Set.empty[GameState]

      @inline
      def next(room: Int, roomPosition: Int): Unit =
        freeCorridorPositions
          .map(targetPos => makeMove(room, roomPosition, targetPos))
          .foreach(_.foreach(moves.addOne))

      // move into corridor scenarios
      for((r, ri) <- rooms.zipWithIndex) {
        // move each top node into the corridor, unless all its successors are in their final place.
        for((roomMember, roomPosition) <- r.zipWithIndex) {
          if(roomMember.isDefined) {
            // if we are already in our room, only move to the corridor when we have others behind us that are not
            if(roomMember.get == ownsRoom(ri)) {
              val successorsAreInPlace = roomIsReady(ownsRoom(ri), r.drop(roomPosition + 1))
              if(! successorsAreInPlace) {
                next(ri, roomPosition)//.foreach(_.foreach(moves.addOne))
              }
            } else {
              next(ri, roomPosition)
            }
          }
        }
      }
      // Move into room scenario
      for((c, ci) <- corridor.zipWithIndex) {
        if(c.isDefined) {
          val r = rooms(targetRoom(c.get))
          // check if there is an empty spot in the correct room
          r.zipWithIndex.reverse.collectFirst {
            case (None, roomPosition) =>
              if (roomIsReady(c.get, r.drop(roomPosition + 1))) {
                makeMove(targetRoom(c.get), roomPosition, ci).foreach(moves.addOne)
              }
          }
        }
      }
      moves
    }
  }

  def roomIsReady(expectedInhabitant: Char, remainder: Vector[Option[Char]]): Boolean = {
    for(r <- remainder) {
      if(r.isEmpty) {
        return false
      }
      if(r.get != expectedInhabitant) {
        return false
      }
    }
    true
  }
}
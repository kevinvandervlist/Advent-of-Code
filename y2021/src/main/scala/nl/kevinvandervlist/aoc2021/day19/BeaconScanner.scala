package nl.kevinvandervlist.aoc2021.day19

import java.util.stream.Collectors
import scala.collection.mutable
import scala.jdk.CollectionConverters
import scala.util.control.*
import scala.util.control.Breaks.{break, breakable}

object BeaconScanner {
  private val minOverlap = 12

  def one(in: List[String]): Int = {
    val (beacons, scanners) = scannerLocations(parseScannerBlocks(in))
    beacons.size
  }

  def two(in: List[String]): Int = {
    val (beacons, scanners) = scannerLocations(parseScannerBlocks(in))
    scanners.toList.combinations(2).map { case a :: b :: Nil => a.manhattanDistance(b) }.max
  }

  private def scannerLocations(allScanners: List[Scanner]): (Set[Point3D], Set[Point3D]) = {
    var worldWithBeacons: Scanner = allScanners.head
    val worldWithScanners = mutable.Set(Point3D(0, 0, 0))
    var remainingScanners = allScanners.tail
    while(remainingScanners.nonEmpty) {
      breakable {
        for (sc <- remainingScanners) {
          for (r <- sc.all24Rotations) {
            worldWithBeacons.otherScannerRelativeCoordinate(r) match {
              case Some(relativePosition) =>
                worldWithScanners.add(relativePosition)
                worldWithBeacons = worldWithBeacons.merge(r, relativePosition)
                remainingScanners = remainingScanners.filterNot(_ == sc)
                break // really gross but needed because otherwise we might add multiple mirrored instances.
              case _ => // not found a match yet
            }
          }
        }
      }
    }
    worldWithBeacons.coordinates.toSet -> worldWithScanners.toSet
  }

  private def parseScannerBlocks(in: List[String]): List[Scanner] = in
    .appended("")
    .foldLeft((List.empty[Scanner], List.empty[String])) {
      case ((scanners, stack), line) => line match {
        case "" => (parseScanner(stack.reverse) :: scanners) -> List.empty
        case _ => scanners -> (line :: stack)
      }
  }._1.reverse

  private def parseScanner(in: List[String]): Scanner = {
    val scannerRegex = """--- scanner (\d+) ---""".r
    val coordinateRegex = """(-?\d+),(-?\d+),(-?\d+)""".r
    val scannerRegex(n) = in.head
    Scanner(n.toInt, in.tail.map(r => {
      val coordinateRegex(x, y, z) = r
      Point3D(x.toInt, y.toInt, z.toInt)
    }))
  }
}

case class Scanner(num: Int, coordinates: List[Point3D]) {
  def all24Rotations: List[Scanner] = {
    val rotatedCoords = coordinates.map(c => c -> c.all24Rotations.toList)
    val all = (0 to 23).toList.map(idx => {
      Scanner(num, rotatedCoords.map(t => t._2(idx)))
    })
    // REALLY make sure we get 24 sides...
    assert(all.toSet.size == 24)
    all
  }

  def otherScannerRelativeCoordinate(other: Scanner): Option[Point3D] = {
    val map = mutable.Map.empty[Point3D, Int]
    for(c <- coordinates) {
      for(co <- other.coordinates) {
        // Compute the delta, as to compensate for the offset of one sensor to another (they are relative positions)
        val delta = c.subtract(co)
        // count the number of 'matches' for each coordinate.
        // We need at least 12 for it to 'count'.
        map.put(delta, map.getOrElse(delta, 1) + 1)
      }
    }
    map.collectFirst {
      case (p, n) if n >= 12 => p
    }
  }

  def merge(other: Scanner): Scanner =
    copy(coordinates = coordinates ++ other.coordinates)

  def merge(other: Scanner, relativePosition: Point3D): Scanner =
    copy(coordinates = coordinates ++ other.coordinates.map(p => p.sum(relativePosition)))
}

case class Point3D(x: Int, y: Int, z: Int) {
  // https://stackoverflow.com/q/16452383
  def all24Rotations: List[Point3D] = {
    var current = this
    val rotations = mutable.ListBuffer.empty[Point3D]
    for(c <- 0 to 1) {
      for(s <- 0 to 2) {
        current = current.roll
        rotations.addOne(current)
        for(i <- 0 to 2) {
          current = current.turn
          rotations.addOne(current)
        }
      }
      // aargh you would not make this up
      current = current.roll.turn.roll
    }
    rotations.toList
  }

  def roll: Point3D = Point3D(x, z, -y)

  def turn: Point3D = Point3D(-y, x, z)

  def subtract(other: Point3D): Point3D =
    Point3D(x - other.x, y - other.y, z - other.z)

  def sum(other: Point3D): Point3D =
    Point3D(x + other.x, y + other.y, z + other.z)

  def manhattanDistance(other: Point3D): Int =
    Math.abs(x - other.x) + Math.abs(y - other.y) + Math.abs(z - other.z)
}
package nl.kevinvandervlist.aoc2021.day22

import nl.kevinvandervlist.aoc2021.day19.Point3D
import nl.kevinvandervlist.aoc2021.day22.ReactorReboot.parseInstructions

import scala.collection.mutable

object ReactorReboot {
  def one(in: List[String]): Long = parseInstructions(in)
    .filter(_.isPartOneCube)
    .foldLeft(mutable.Set.empty[Point3D]) {
      case (set, o: On) => set.addAll(o.allPoints)
      case (set, o: Off) => set.diff(o.allPoints.toSet)
    }.size

  // that would have been too easy...
  def two(in: List[String]): Long = {
    val allCuboids = mutable.ListBuffer.empty[Cuboid]
    for(currentInstruction <- parseInstructions(in)) {
      val current = currentInstruction.cuboid
      var toAdd = List.empty[Cuboid]

      for(c <- allCuboids) {
        // so any overlap we would intersect when using a set, and remove.
        // However, because later cuboids might intersect again, we just keep them with a native volume
        if(current.isOverlapping(c)) {
          val next = current.intersect(c)
          toAdd = next :: toAdd
        }
      }

      allCuboids.addAll(toAdd)
      if(current.isOn) {
        allCuboids.addOne(current)
      }
    }
    allCuboids.map(_.volume).sum
  }

  private def parseInstructions(in: List[String]): List[Instruction] = {
    val re = """(\w+) x=(-?[0-9]+)..(-?[0-9]+),y=(-?[0-9]+)..(-?[0-9]+),z=(-?[0-9]+)..(-?[0-9]+)""".r
    in.map(line => {
      val re(onOff,xa,xb,ya,yb,za,zb) = line
      onOff match {
        case "on" => On(Point3D(xa.toInt, ya.toInt, za.toInt), Point3D(xb.toInt, yb.toInt, zb.toInt))
        case "off" => Off(Point3D(xa.toInt, ya.toInt, za.toInt), Point3D(xb.toInt, yb.toInt, zb.toInt))
      }
    })
  }
}

sealed trait Instruction {
  def start: Point3D
  def end: Point3D

  def isPartOneCube: Boolean = List(
    start.x > -50,
    end.x < 50,
    start.y > -50,
    end.y < 50,
    start.z > -50,
    end.z < 50
  ).forall(_ == true)

  def allPoints: Iterable[Point3D] = for {
    x <- start.x to end.x
    y <- start.y to end.y
    z <- start.z to end.z
  } yield Point3D(x, y, z)

  def isOn: Boolean
  def cuboid: Cuboid = Cuboid(isOn, start, end)
}
case class On(start: Point3D, end: Point3D) extends Instruction {
  override def isOn: Boolean = true
}
case class Off(start: Point3D, end: Point3D) extends Instruction {
  override def isOn: Boolean = false
}

case class Cuboid(isOn: Boolean, a: Point3D, b: Point3D) {
  @inline
  def width: Long = math.abs(a.x.toLong - b.x.toLong) + 1
  @inline
  def height: Long = math.abs(a.y.toLong - b.y.toLong) + 1
  @inline
  def depth: Long = math.abs(a.z.toLong - b.z.toLong) + 1

  def volume: Long = if (isOn) {
    width * height * depth
  } else {
    width * height * depth * -1L
  }

  def isOverlapping(other: Cuboid): Boolean = {
    val intersection = intersect(other)
    (intersection.a.x <= intersection.b.x) &&
      (intersection.a.y <= intersection.b.y) &&
      (intersection.a.z <= intersection.b.z)
  }

  def intersect(other: Cuboid): Cuboid = {
    val ia = Point3D(
      Math.max(a.x, other.a.x),
      Math.max(a.y, other.a.y),
      Math.max(a.z, other.a.z),
    )
    val ib = Point3D(
      Math.min(b.x, other.b.x),
      Math.min(b.y, other.b.y),
      Math.min(b.z, other.b.z),
    )
    Cuboid(! other.isOn, ia, ib)
  }
}

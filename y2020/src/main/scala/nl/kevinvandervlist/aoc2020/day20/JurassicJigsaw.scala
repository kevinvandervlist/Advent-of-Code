package nl.kevinvandervlist.aoc2020.day20

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex

object JurassicJigsaw {
  def one(in: List[String]): Long = {
    val alignmentMap: Map[Tile, List[Tile]] = findAlignmentMap(in)
    val corners = alignmentMap.filter(_._2.size == 2)
    corners.map(_._1.id.toLong).product
  }

  def two(in: List[String]): Long = {
    val alignmentMap: Map[Tile, List[Tile]] = findAlignmentMap(in)
    val reconstructed = reconstruct(alignmentMap)
    val singleImage = combine(reconstructed)
    val monster = List(
      "                  # ",
      "#    ##    ##    ###",
      " #  #  #  #  #  #   "
    ).map(_.toList)
    val monsterPoundCount = monster.flatten.count(_ == '#')
    val options: Map[Tile, Int] = singleImage
      .variations
      .map(t => t -> count(monster, t))
      .toMap

    options.map {
      case (k, v) => k.pixels.flatten.count(_ == '#') - (v * monsterPoundCount)
    }.min
  }

  private def count(monster: List[List[Char]], image: Tile): Int = {
    val monsterHeight = monster.length
    val monsterWidth = monster.head.length
    val monsterRegex = monster
      .map(m => new Regex(m.mkString("").replace(' ', '.')))
    var monsterCount = 0

    var y = 0
    var x = 0
    while(y < (image.pixels.length - monsterHeight)) {
      x = 0
      while(x < (image.pixels.head.length - monsterWidth)) {
        if(monsterRegex.head.matches(image.pixels(y).slice(x, x + monsterWidth).mkString(""))) {
          val isMonster = (1 until monsterRegex.length).map(id =>
            monsterRegex(id).matches(image.pixels(y + id).slice(x, x + monsterWidth).mkString(""))
          ).forall(identity)
          if(isMonster) {
            monsterCount += 1
          }
        }
        x += 1
      }
      y += 1
    }
    monsterCount
  }

  private def combine(tiles: List[List[Tile]]): Tile = {
    val image: ListBuffer[ListBuffer[Char]] = new ListBuffer[ListBuffer[Char]]
    val lines = (0 until (tiles.length * (tiles.head.head.pixels.length - 2))).toList
    val tileLen = tiles.head.head.pixels.length - 2
    for(_ <- lines) {
      image.addOne(new ListBuffer[Char])
    }

    for(l <- lines) {
      val tilesRow = tiles(l / tileLen).map(t => t.copy(pixels = t.pixels.drop(1).dropRight(1)))
      val tileRow = l % tileLen
      for(tidx <- tilesRow.indices) {
        val view = tilesRow(tidx).pixels(tileRow).drop(1).dropRight(1)
        image(l).addAll(view)
      }
    }
    Tile(0, image.map(_.toList).toList)
  }


  private def reconstruct(_alignmentMap: Map[Tile, List[Tile]]): List[List[Tile]] = {
    val alignmentMap = _alignmentMap.map {
      case (k, v) => k.id -> v
    }
    val corners: Map[Tile, List[Tile]] = _alignmentMap.filter(_._2.size == 2)
    val image = new ListBuffer[ListBuffer[Tile]]
    val topLeft = findTopLeft(corners)
    var y = 0
    val size = Math.sqrt(alignmentMap.size).toInt
    var cur: Option[Tile] = Some(topLeft)

    while(y < size && cur.isDefined) {
      var x = 1
      image.addOne(new ListBuffer[Tile])
      image(y).addOne(cur.get) // Add first tile of the row
      while(x < size) {
        val options = alignmentMap(cur.get.id).flatMap(_.variations)
        val next = alignmentMap(cur.get.id).flatMap(_.variations).find(cur.get.alignRight)
        cur = next
        next.foreach(image(y).addOne)
        x += 1
      }
      cur = alignmentMap(image(y).head.id).flatMap(_.variations).find(image(y).head.alignBottom)
      y += 1
    }
    image.map(_.toList).toList
  }

  private def findTopLeft(value: Map[Tile, List[Tile]]): Tile = {
    val options: Iterable[(Tile, Tile, Tile)] = value.flatMap {
      case (t, a :: b :: Nil) =>
        val tVariations = t.variations
        val aVariations = a.variations
        val bVariations = b.variations
        val combinations = for {
          _t <- tVariations
          _a <- aVariations
          _b <- bVariations
        } yield (_t, _a, _b)
        combinations.filter {
          case (t, na, nb) => t.alignRight(na) && t.alignBottom(nb)
        }
    }
    options.head._1
  }

  private def findAlignmentMap(in: List[String]): Map[Tile, List[Tile]] = {
    val tiles = parse(in)
    val alignmentMap: mutable.Map[Tile, List[Tile]] = mutable.Map.empty
    for(t <- tiles) {
      val remaining = tiles.filterNot(_.id == t.id)
      alignmentMap.put(t, remaining.filter(t.alignsWith))
    }
    alignmentMap.toMap
  }

  private def parse(lines: List[String]): List[Tile] = {
    val id = "Tile (\\d+):".r
    val tileOffset = 12
    val tiles = new ListBuffer[Tile]
    var idx = 0
    while(idx < lines.length) {
      val id(_id) = lines(idx)
      tiles.addOne(Tile(_id.toInt,
        lines.slice(idx + 1, idx + tileOffset - 1).map(_.toList)
      ))
      idx += tileOffset
    }
    tiles.toList
  }
}

private case class Tile(id: Int, pixels: List[List[Char]]) {
  def alignments(other: Tile): List[(Tile, Tile)] = (for {
      r1 <- variations
      r2 <- other.variations
    } yield r1 -> r2).filter {
      case (r1, r2) => r1.alignsWithoutChange(r2)
    }

  def alignsWith(other: Tile): Boolean =
    alignments(other).nonEmpty

  def variations: List[Tile] = {
    val result = Array(this, null, null, null)
    var idx = 1
    // rotations
    while(idx <= 3) {
      result(idx) = result(idx - 1).rotate
      idx += 1
    }
    // flipped
    val flipped = result.map(_.flip)
    result.toList ++ flipped
  }

  private def rotate: Tile =
    Tile(id, pixels.transpose.map(_.reverse))

  private def flip: Tile =
    Tile(id, pixels.reverse)

  private def alignsWithoutChange(other: Tile): Boolean =
    alignTop(other)

  @inline
  def alignTop(other: Tile): Boolean =
    pixels.head == other.pixels.last

  @inline
  def alignRight(other: Tile): Boolean =
    pixels.map(_.last) == other.pixels.map(_.head)

  @inline
  def alignBottom(other: Tile): Boolean =
    pixels.last == other.pixels.head

  @inline
  def alignLeft(other: Tile): Boolean =
    pixels.map(_.head) == other.pixels.map(_.last)

  override def toString: String = {
    val sb = new StringBuilder()
    sb.append(id).append("\n")
    sb.append(pixels.map(_.mkString("")).mkString("\n"))
    sb.toString()
  }
}
package nl.kevinvandervlist.aoc2020.day20

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

    val total = singleImage.pixels.map(_.count(_ == '#')).sum
    singleImage
      .variations
      .iterator
      .map(count(monster, _))
      .map(v => total - (v * monsterPoundCount))
      .min
  }

  private def count(monster: List[List[Char]], image: Tile): Int = {
    val monsterHeight = monster.length
    val monsterWidth = monster.head.length
    // Terrible hack ...
    val monsterRegex = monster
      .map(m => new Regex(m.mkString("").replace(' ', '.')))
      .toArray
      .zipWithIndex
    var monsterCount = 0

    var y = 0
    var x = 0
    while(y < (image.pixels.length - monsterHeight)) {
      x = 0
      while(x < (image.pixels.head.length - monsterWidth)) {
        if(monsterRegex.forall {
          case (r, offset) => r.matches(image.pixels(y + offset).slice(x, x + monsterWidth).mkString(""))
        }) {
          monsterCount += 1
        }
        x += 1
      }
      y += 1
    }
    monsterCount
  }

  private def combine(_tiles: List[List[Tile]]): Tile = {
    val len = _tiles.head.head.pixels.length
    // Remove all the borders
    val tiles = _tiles.map(_.map(t => t.copy(
      pixels = t.pixels.slice(1, len - 1).map(_.slice(1, len - 1))
    )))
    val tileLen = tiles.head.head.width
    val lines = (0 until (tiles.length * tileLen)).toList

    val image = lines.foldLeft(new ListBuffer[ListBuffer[Char]]) {
      case (image, l) =>
        image.addOne(new ListBuffer[Char])
        val tilesRow = tiles(l / tileLen)
        val tileRow = l % tileLen
        tilesRow.indices.foldLeft(image) {
          case (acc, tidx) =>
            acc(l).addAll(tilesRow(tidx).pixels(tileRow))
            acc
        }
        image
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
        val next = alignmentMap(cur.get.id).iterator.flatMap(_.variations).find(cur.get.alignRight)
        cur = next
        next.foreach(image(y).addOne)
        x += 1
      }
      cur = alignmentMap(image(y).head.id).iterator.flatMap(_.variations).find(image(y).head.alignBottom)
      y += 1
    }
    image.map(_.toList).toList
  }

  private def findTopLeft(value: Map[Tile, List[Tile]]): Tile = {
    val options: Iterable[(Tile, Tile, Tile)] = value.flatMap {
      case (t, a :: b :: Nil) =>
        for {
          _t <- t.variations
          _a <- a.variations
          if _t.alignRight(_a)
          _b <- b.variations
          if _t.alignBottom(_b)
        } yield (_t, _a, _b)
    }
    options.head._1
  }

  private def findAlignmentMap(in: List[String]): Map[Tile, List[Tile]] = {
    val tiles = parse(in)
    tiles.foldLeft(List.empty[(Tile, List[Tile])]) {
      case (acc, t) => (t-> tiles.iterator.filterNot(_.id == t.id).filter(t.alignsWith).toList) :: acc
    }.toMap
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
  @inline
  def width: Int = pixels.head.length

  @inline
  def height: Int = pixels.head.length

  def alignments(other: Tile): Iterator[(Tile, Tile)] = (for {
      r1 <- variations
      r2 <- other.variations
    } yield r1 -> r2).filter {
      case (r1, r2) => r1.alignsWithoutChange(r2)
    }

  def variations: Iterator[Tile] = {
    val result = Array(this, null, null, null)
    var idx = 1
    // rotations
    while(idx <= 3) {
      result(idx) = result(idx - 1).rotate
      idx += 1
    }
    LazyList.from(result)
      .appendedAll(
        LazyList.from(result).map(_.flip)
      ).iterator
  }

  @inline
  private def rotate: Tile =
    Tile(id, pixels.transpose.map(_.reverse))

  @inline
  private def flip: Tile =
    Tile(id, pixels.reverse)

  @inline
  def alignsWith(other: Tile): Boolean =
    alignments(other).nonEmpty

  @inline
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
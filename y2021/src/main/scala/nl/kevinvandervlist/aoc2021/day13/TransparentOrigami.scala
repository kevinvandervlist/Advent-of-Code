package nl.kevinvandervlist.aoc2021.day13

import nl.kevinvandervlist.aoc.RectangularGrid

object TransparentOrigami {
  def one(in: List[String]): Int = {
    val fold = foldInstructions(in).head
    val g = foldPaper(grid(in), fold)
    g.count(_ == '#').toInt
  }

  def two(in: List[String]): String = {
    val instr = foldInstructions(in)
    val origami = instr.foldLeft(grid(in)) {
      case (p, i) => foldPaper(p, i)
    }
    origami.print
  }

  private def foldInstructions(in: List[String]): List[(Char, Int)] = {
    val re = """fold along ([xy])=(\d+)""".r
    in
      .filter(re.matches)
      .map(l => {
        val re(axis, n) = l
        axis.head -> n.toInt
      })
  }

  private def grid(in: List[String]): RectangularGrid[Char] = {
    val coords = in
      .takeWhile(_.contains(','))
      .map(s => {
        val splitted = s.split(',')
        splitted(0).toInt -> splitted(1).toInt
      })

    val xMax = coords.map(_._1).max + 1 // starts at 0
    val yMax = coords.map(_._2).max + 1
    coords.foldLeft(RectangularGrid.apply(xMax, yMax, '.')) {
      case (g, (x, y)) => g.set(x, y, c => '#')
    }
  }

  private def foldPaper(paper: RectangularGrid[Char], instruction: (Char, Int)): RectangularGrid[Char] =
    instruction match {
      case ('y', l) => foldY(paper, l)
      case ('x', c) => foldX(paper, c)
    }

  private def foldY(paper: RectangularGrid[Char], line: Int): RectangularGrid[Char] = {
    // horizontal, fold up, so take half of paper
    val topHalf = RectangularGrid(paper.elements.take(line))
    val flippedBottom = RectangularGrid(paper.elements.drop(line + 1)).flipTop

    var largest = paper
    var smallest = paper
    if(flippedBottom.elements.length > topHalf.elements.length) {
      largest = flippedBottom
      smallest = topHalf
    } else {
      largest = topHalf
      smallest = flippedBottom
    }
    // to align with the 'fold' line at (0,0) while merging paper that is not of equal size
    // flip, then select largest, then merge, then flip again
    foldResult(smallest.flipTop, largest.flipTop).flipTop
  }

  private def foldX(paper: RectangularGrid[Char], column: Int): RectangularGrid[Char] = {
    // vertical, fold left
    val left = RectangularGrid(paper.elements.map(_.take(column)))
    val flippedRight = RectangularGrid(paper.elements.map(_.drop(column + 1))).flipLeft

    var largest = paper
    var smallest = paper
    if(left.elements.head.length > flippedRight.elements.head.length) {
      largest = left
      smallest = flippedRight
    } else {
      largest = flippedRight
      smallest = left
    }
    // transpose/flip largest, then select largest, then merge, then flip/transpose again
    foldResult(smallest.transpose.flipTop, largest.transpose.flipTop).flipTop.transpose
  }

  private def foldResult(smallest: RectangularGrid[Char], largest: RectangularGrid[Char]): RectangularGrid[Char] =
    smallest
      .allCoordinates
      .collect { case c if smallest.get(c).contains('#') => c }
      .foldLeft(largest) {
        case (p, c) => p.set(c, o => '#')
      }
}

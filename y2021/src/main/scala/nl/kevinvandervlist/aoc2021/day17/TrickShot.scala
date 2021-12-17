package nl.kevinvandervlist.aoc2021.day17

import nl.kevinvandervlist.aoc.Point

object TrickShot {
  private val re = """target area: x=(-?\d+)..(-?\d+), y=(-?\d+)..(-?\d+)""".r
  def one(in: String): Int = {
    val re(xMin, xMax, yMin, yMax) = in
    velocitiesThatLandInBox(xMin.toInt, xMax.toInt, yMin.toInt, yMax.toInt).map {
      case (x, y) => findMaxVelocity(xMin.toInt, xMax.toInt, yMin.toInt, yMax.toInt, x, y)
    }.max
  }

  def two(in: String): Int = {
    val re(xMin, xMax, yMin, yMax) = in
    velocitiesThatLandInBox(xMin.toInt, xMax.toInt, yMin.toInt, yMax.toInt).size
  }

  private def velocitiesThatLandInBox(xMin: Int, xMax: Int, yMin: Int, yMax: Int): LazyList[(Int, Int)] =
    // very simple heuristic of limits based on guesses of input data
    LazyList.range(1, xMax + 1)
      .flatMap(x => LazyList.range(yMin, -1 * yMin)
      .map(y => x -> y))
      .collect {
        case (x, y) if landsInBox(xMin, xMax, yMin, yMax, x, y) => x -> y
      }

  // I assume it eventually lands in the box when this is called
  private def findMaxVelocity(xMin: Int, xMax: Int, yMin: Int, yMax: Int, x: Int, y: Int): Int =
    trajectory(x, y, pastHitBox(xMin, xMax, yMin, yMax, _)).maxBy(_.y).y

  private def landsInBox(xMin: Int, xMax: Int, yMin: Int, yMax: Int, x: Int, y: Int): Boolean =
    trajectory(x, y, pastHitBox(xMin, xMax, yMin, yMax, _))
      .exists(inHitbox(xMin, xMax, yMin, yMax, _))

  private def trajectory(initialXVelocity: Int, initialYVelocity: Int, pastHitBoxPred: Point => Boolean): LazyList[Point] = {
    var xVelocity = initialXVelocity
    var yVelocity = initialYVelocity
    var xCurrent = 0
    var yCurrent = 0
    var first_takeWhileHack = true
    LazyList.continually({ // do a step in this block as soon as a new element is requested
      xCurrent = xCurrent + xVelocity
      yCurrent = yCurrent + yVelocity
      if(xVelocity > 0) {
        xVelocity -= 1
      }
      if(xVelocity < 0) {
        xVelocity += 1
      }
      yVelocity -= 1
      Point(xCurrent, yCurrent)
    })// a hack to do takeWhile until the predicate holds, but to also keep the affected element
      // i.e. takeWhileInclusive
      .takeWhile(point => {
      var hasNotPassedBoxYet = ! pastHitBoxPred(point)
      if(! hasNotPassedBoxYet && first_takeWhileHack) {
        first_takeWhileHack = false
        hasNotPassedBoxYet = true
      }
      hasNotPassedBoxYet
    })
  }

  private def inHitbox(xMin: Int, xMax: Int, yMin: Int, yMax: Int, p: Point): Boolean =
    p.x >= xMin && p.x <= xMax && p.y >= yMin && p.y <= yMax

  private def pastHitBox(xMin: Int, xMax: Int, yMin: Int, yMax: Int, p: Point): Boolean =
    p.x > xMax || p.y < yMin
}

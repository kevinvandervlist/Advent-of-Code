package nl.kevinvandervlist.aoc2015.day02

object NoMath {
  def one(in: List[String]): Int = boxes(in)
    .map(_.total)
    .sum

  def two(in: List[String]): Int = boxes(in)
    .map(_.ribbon)
    .sum

  private def boxes(in: List[String]): List[WrappedBox] = in
    .map(_.split('x'))
    .map(ms => WrappedBox(ms(0).toInt, ms(1).toInt, ms(2).toInt))
}

private case class WrappedBox(l: Int, w: Int, h: Int) {
  def areas: List[Int] = List(l*w, l*h, w*h)
  def slack: Int = areas.min
  def total: Int = areas.sum * 2 + slack
  def sides: List[Int] = List(l, w, h)
  def ribbon: Int = {
    val around = List(
      l+l+w+w,
      l+l+h+h,
      w+w+h+h
    ).min
    val bow = l * w * h
    around + bow
  }
}
package nl.kevinvandervlist.aoc2021.day02

object Dive {
  def one(steps: List[String]): Long = {
    val finalPosition = steps.map(parse.apply).foldLeft(Position(0, 0))({
      case (pos, step) => step.update(pos)
    })
    finalPosition.vertical * finalPosition.horizontal
  }

  def two(steps: List[String]): Long = {
    val finalPosition = steps.map(parse.apply).foldLeft(PositionWithAim(0, 0, 0))({
      case (pos, step) => step.update(pos)
    })
    finalPosition.vertical * finalPosition.horizontal
  }

  private def parse(step: String): Step = {
    val splitted = step.split(" ")
    splitted(0) match {
      case "forward" => Forward(splitted(1).toInt)
      case "down" => Down(splitted(1).toInt)
      case "up" => Up(splitted(1).toInt)
    }
  }
}

case class Position(horizontal: Long, vertical: Long)
case class PositionWithAim(horizontal: Long, vertical: Long, aim: Long)

private sealed trait Step {
  def update(pos: Position): Position
  def update(pos: PositionWithAim): PositionWithAim
}
case class Forward(horizontal: Int) extends Step {
  def update(pos: Position): Position =
    pos.copy(horizontal = pos.horizontal + horizontal)

  def update(pos: PositionWithAim): PositionWithAim =
    pos.copy(
      horizontal = pos.horizontal + horizontal,
      vertical = pos.vertical + (pos.aim * horizontal)
    )
}
case class Down(vertical: Int) extends Step {
  def update(pos: Position): Position =
    pos.copy(vertical = pos.vertical + vertical)

  def update(pos: PositionWithAim): PositionWithAim =
    pos.copy(aim = pos.aim + vertical)
}
case class Up(vertical: Int) extends Step{
  def update(pos: Position): Position =
    pos.copy(vertical = pos.vertical - vertical)

  def update(pos: PositionWithAim): PositionWithAim =
    pos.copy(aim = pos.aim - vertical)
}

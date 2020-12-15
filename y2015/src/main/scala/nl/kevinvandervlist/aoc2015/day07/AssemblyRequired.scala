package nl.kevinvandervlist.aoc2015.day07

import scala.annotation.tailrec

object AssemblyRequired {
  def one(in: List[String]): Map[String, Int] =
    wire(in.map(parse))

  def two(in: List[String]): Int = {
    val value = one(in)("a")
    val patchedInstructions = in.map(parse).collect {
      case Const(_, "b") => Const(value.toString, "b")
      case otherwise => otherwise
    }
    wire(patchedInstructions)("a")
  }

  private def wire(instructions: List[Instruction]): Map[String, Int] = {
    rec(Map.empty, instructions)
  }

  @tailrec
  private def rec(state: Map[String, Int], instr: List[Instruction]): Map[String, Int] = instr match {
    case Nil => state
    case _ =>
      val (ready, notYet) = instr.partition(isReady(state, _))
      val updatedState = ready.foldLeft(state) {
        case (m, i) => i.effect(m)
      }
      rec(updatedState, notYet)
  }

  private def isReady(state: Map[String, Int], instruction: Instruction): Boolean =
    instruction.inRegisters.forall(state.contains)

  private val const = "(\\w+) -> (\\w+)".r
  private val not = "NOT (\\w+) -> (\\w+)".r
  private val binary = "(\\w+) (\\w+) (\\w+) -> (\\w+)".r

  private def parse(line: String): Instruction = {
    if(const.matches(line)) {
      val const(v, out) = line
      return Const(v, out)
    }
    if(not.matches(line)) {
      val not(a, out) = line
      return Not(a, out)
    }
    if(binary.matches(line)) {
      val binary(a, op, b, out) = line
      return op match {
        case "AND" => And(a, b, out)
        case "OR" => Or(a, b, out)
        case "LSHIFT" => LShift(a, b, out)
        case "RSHIFT" => RShift(a, b, out)
      }
    }
    ???
  }
}

private sealed trait Instruction {
  def in: List[String]
  def inRegisters: List[String] = in.filter(_.toIntOption.isEmpty)
  protected def read(name: String, circuit: Map[String, Int]): Int = name.toIntOption match {
    case Some(i) => i
    case _ => circuit(name)
  }
  def out: String
  def effect(circuit: Map[String, Int]): Map[String, Int]
}

private case class Const(a: String, out: String) extends Instruction {
  override def in: List[String] = List(a)
  override def effect(circuit: Map[String, Int]): Map[String, Int] =
    circuit + (out -> read(a, circuit))
}

private case class And(a: String, b: String, out: String) extends Instruction {
  override def in: List[String] = List(a, b)
  override def effect(circuit: Map[String, Int]): Map[String, Int] =
    circuit + (out -> (read(a, circuit) & read(b, circuit)))
}

private case class Or(a: String, b: String, out: String) extends Instruction {
  override def in: List[String] = List(a, b)
  override def effect(circuit: Map[String, Int]): Map[String, Int] =
    circuit + (out -> (read(a, circuit) | read(b, circuit)))
}

private case class LShift(a: String, b: String, out: String) extends Instruction {
  override def in: List[String] = List(a, b)
  override def effect(circuit: Map[String, Int]): Map[String, Int] =
    circuit + (out -> (read(a, circuit) << read(b, circuit)))
}

private case class RShift(a: String, b: String, out: String) extends Instruction {
  override def in: List[String] = List(a, b)
  override def effect(circuit: Map[String, Int]): Map[String, Int] =
    circuit + (out -> (read(a, circuit) >> read(b, circuit)))
}

private case class Not(a: String, out: String) extends Instruction {
  override def in: List[String] = List(a)
  // No unsigned types, so force as char (which is unsigned 16 bit)
  override def effect(circuit: Map[String, Int]): Map[String, Int] =
    circuit + (out -> (~ read(a, circuit)).asInstanceOf[Char])
}
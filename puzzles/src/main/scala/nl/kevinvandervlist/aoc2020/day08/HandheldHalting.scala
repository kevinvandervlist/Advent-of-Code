package nl.kevinvandervlist.aoc2020.day08

import scala.collection.mutable
import scala.util.Try

object HandheldHalting {
  def one(in: List[String]): Int =
    executeUntilLoop(in.map(parseInstruction))

  def two(in: List[String]): Int = {
    def ok(p: List[Instruction]): Boolean = Try {
      executeUntilLast(p)
    }.map(_ => true).getOrElse(false)

    allPatches(in.map(parseInstruction))
      .collectFirst {
        case p: List[Instruction] if ok(p) => executeUntilLast(p)
      }.get
  }

  private def parseInstruction(raw: String): Instruction = {
    val x = raw.split(' ')
    x(0) match {
      case "acc" => Acc(x(1).toInt)
      case "jmp" => Jmp(x(1).toInt)
      case "nop" => Nop(x(1).toInt)
    }
  }

  private def execute(program: List[Instruction], shouldHalt: Int => Boolean, effect: Int => Unit): Int = {
    var state = 0
    var idx = 0
    while(! shouldHalt(idx)) {
      effect(idx)
      program(idx) match {
        case Acc(inc) => state += inc; idx +=1
        case Jmp(cnt) => idx += cnt
        case Nop(_) => idx += 1
      }
    }
    state
  }

  private def executeUntilLast(program: List[Instruction]): Int = {
    val seen = new mutable.HashSet[Int]()
    execute(program, _ == program.size, idx => {
      assert(! seen.contains(idx))
      seen.add(idx)
    })
  }

  private def executeUntilLoop(program: List[Instruction]): Int = {
    val seen = new mutable.HashSet[Int]()
    execute(program, seen.contains, idx => seen.add(idx))
  }

  // Somewhere in the program, either a jmp is supposed to be a nop, or a nop is supposed to be a jmp.
  private def allPatches(program: List[Instruction]): LazyList[List[Instruction]] = LazyList.unfold(0)(idx => {
    if(idx > program.length) {
      None
    } else {
      val patched = program(idx) match {
        case Jmp(cnt) => program.updated(idx, Nop(cnt))
        case Nop(cnt) => program.updated(idx, Jmp(cnt))
        case _ => List.empty
      }
      Some(patched -> (idx + 1))
    }
  }).filterNot(_.isEmpty)
}

private sealed trait Instruction
private case class Acc(cnt: Int) extends Instruction
private case class Jmp(cnt: Int) extends Instruction
private case class Nop(cnt: Int) extends Instruction

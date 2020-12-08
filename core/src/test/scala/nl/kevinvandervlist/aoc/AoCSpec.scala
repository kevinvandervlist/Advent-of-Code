package nl.kevinvandervlist.aoc

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

trait AoCSpec extends AnyWordSpec with Matchers {
  /** A string with the example of this challenge */
  def example: String

  /** A reference to the resource name with input of this challenge */
  def resource: String =
    s"day-${getClass.getPackageName.takeRight(2)}-input"

  /** The resource as lines */
  def inputAsLines: List[String] =
    IO.resourceByLines(resource)

  /** The resource as string */
  def inputAsString: String =
    IO.resource(resource)

  /** The example as lines */
  def exampleAsLines: List[String] =
    example.split(System.lineSeparator()).toList

  /** Validation for the example of part one */
  def examplePartOne(): Any

  /** Validation for the input of part one */
  def assignmentPartOne(): Any

  /** Validation for the example of part two */
  def examplePartTwo(): Any

  /** Validation for the input of part two */
  def assignmentPartTwo(): Any

  "evaluate part 1" should {
    "validate example" in {
      examplePartOne()
    }
    "validate assignment" in {
      assignmentPartOne()
    }
  }
  "evaluate part 2" should {
    "validate example" in {
      examplePartTwo()
    }
    "validate assignment" in {
      assignmentPartTwo()
    }
  }
}

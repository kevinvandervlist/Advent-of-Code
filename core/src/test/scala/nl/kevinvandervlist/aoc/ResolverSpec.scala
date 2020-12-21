package nl.kevinvandervlist.aoc

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ResolverSpec extends AnyWordSpec with Matchers {
  "Solving a simple example" should {
    val example = Map(
      0 -> Set("a", "b"),
      1 -> Set("c"),
      2 -> Set("c", "a")
    )
    "give one result" in {
      Resolver.solve(example).size shouldBe 1
    }
    "give expected result" in {
      Resolver.solve(example).head should contain allElementsOf Map(
        0 -> "b",
        1 -> "c",
        2 -> "a"
      )
    }
  }
  "Solving a more complex example" should {
    val example = Map(
      0 -> List("a", "b"),
      1 -> List("c", "a"),
      2 -> List("c", "a")
    )
    "give two results" in {
      Resolver.solve(example).size shouldBe 2
    }
    "give expected result" in {
      Resolver.solve(example) should contain allElementsOf List(Map(
        0 -> "b",
        1 -> "c",
        2 -> "a"
      ), Map(
        0 -> "b",
        1 -> "a",
        2 -> "c"
      ))
    }
  }
}

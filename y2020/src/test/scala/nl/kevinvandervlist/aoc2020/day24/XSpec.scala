package nl.kevinvandervlist.aoc2020.day24

import nl.kevinvandervlist.aoc.AoCSpec

class XSpec extends AoCSpec {
  override def example: String =
    """sesenwnenenewseeswwswswwnenewsewsw
      |neeenesenwnwwswnenewnwwsewnenwseswesw
      |seswneswswsenwwnwse
      |nwnwneseeswswnenewneswwnewseswneseene
      |swweswneswnenwsewnwneneseenw
      |eesenwseswswnenwswnwnwsewwnwsene
      |sewnenenenesenwsewnenwwwse
      |wenwwweseeeweswwwnwwe
      |wsweesenenewnwwnwsenewsenwwsesesenwne
      |neeswseenwwswnwswswnw
      |nenwswwsewswnenenewsenwsenwnesesenew
      |enewnwewneswsewnwswenweswnenwsenwsw
      |sweneswneswneneenwnewenewwneswswnese
      |swwesenesewenwneswnwwneseswwne
      |enesenwswwswneneswsenwnewswseenwsese
      |wnwnesenesenenwwnenwsewesewsesesew
      |nenewswnwewswnenesenwnesewesw
      |eneswnwswnwsenenwnwnwwseeswneewsenese
      |neswnwewnwnwseenwseesewsenwsweewe
      |wseweeenwnesenwwwswnew""".stripMargin

  override def examplePartOne(): Any =
    X.one(exampleAsLines) shouldBe 10

  override def assignmentPartOne(): Any =
    X.one(inputAsLines) shouldBe 465

  override def examplePartTwo(): Any =
    X.two(exampleAsLines) shouldBe 2208

  override def assignmentPartTwo(): Any = {
    X.two(inputAsLines) should be > 3607
  }
}

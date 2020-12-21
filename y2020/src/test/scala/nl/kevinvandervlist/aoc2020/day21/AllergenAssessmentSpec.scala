package nl.kevinvandervlist.aoc2020.day21

import nl.kevinvandervlist.aoc.AoCSpec

class AllergenAssessmentSpec extends AoCSpec {
  override def example: String =
    """mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
      |trh fvjkl sbzzf mxmxvkd (contains dairy)
      |sqjhc fvjkl (contains soy)
      |sqjhc mxmxvkd sbzzf (contains fish)
      |""".stripMargin

  override def examplePartOne(): Any =
    AllergenAssessment.one(exampleAsLines) shouldBe 5

  override def assignmentPartOne(): Any =
    AllergenAssessment.one(inputAsLines) shouldBe 2262

  override def examplePartTwo(): Any =
    AllergenAssessment.two(exampleAsLines) shouldBe "mxmxvkd,sqjhc,fvjkl"

  override def assignmentPartTwo(): Any =
    AllergenAssessment.two(inputAsLines) shouldBe "cxsvdm,glf,rsbxb,xbnmzr,txdmlzd,vlblq,mtnh,mptbpz"
}


package nl.ing.receiptLocation

import org.scalatest.{Matchers, WordSpecLike}

class ItemsExtractorSpec extends WordSpecLike with Matchers  {

  val bla = """(\d+)[,\.](\d\d)""".r

  "regex" should {
    "work" in {
      val matches = "3,16" match {
        case bla(inter, fraction) =>
          println(s"$inter,$fraction")
          true
        case _ =>
          false
      }
     matches shouldBe true

    }
  }
}


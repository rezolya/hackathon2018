package nl.ing.receiptLocation

import nl.ing.receiptLocations.{Rectangle, Vec}
import org.scalatest.{Matchers, WordSpecLike}

class SchemaSpec extends WordSpecLike with Matchers {

  "The rectangles" should {
    "allign with another rectangle if tilted" in {
      val rec1 = Rectangle(Vec(0,0), Vec(2,1), Vec(1,3), Vec(-1, 2))
      val rec2 = Rectangle(Vec(4,2), Vec(6,3), Vec(5,5), Vec(3,4))
      rec1.isOnSameHeight(rec2) shouldBe true
      rec2.isOnSameHeight(rec1) shouldBe true
      rec1.isOnSameHeight(rec1) shouldBe false
    }
    "not allign with another tilted rectangle" in {
      val rec1 = Rectangle(Vec(0,0), Vec(2,1), Vec(1,3), Vec(-1, 2))
      val rec2 = Rectangle(Vec(10,1), Vec(12,1), Vec(11,3), Vec(9,2))
      rec1.isOnSameHeight(rec2) shouldBe false
      rec2.isOnSameHeight(rec1) shouldBe false

    }
  }
}

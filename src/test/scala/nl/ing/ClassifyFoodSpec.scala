package nl.ing

import nl.ing.ClassifyFood.ItemDetails
import nl.ing.model.Item
import nl.ing.model.ItemCategories.grosseries
import org.scalatest.{Matchers, WordSpecLike}

class ClassifyFoodSpec extends WordSpecLike with Matchers  {


  "the food classifier" should {

    "classify" in {
      ClassifyFood.classifyFood(model.transactions)
    }

    "find details" in {
      val item = Item("AH BOLLEN", 1F, "1", grosseries)

      val result = ClassifyFood.getDetails(item)
      println(result)
    }

    "score difference" in {
      val details = ItemDetails("AH APPEL", 0.75F, 100, grosseries)
      val item = Item("AH BOLLEN", 1F, "1", grosseries)
      val dif = details.differenceScore(item)
      println(dif)
    }

    "score difference 2" in {
      val details = ItemDetails("AH APPEL", 0.75F, 100, grosseries)
      val item = Item("APPEL", 0.75F, "1", grosseries)
      val dif = details.differenceScore(item)
      println(dif)
    }

    "load CSV file" in {
      val details: Seq[ItemDetails] = ClassifyFood.ahDB
      details.length should be > 10
      details.foreach(println)
    }
  }
}

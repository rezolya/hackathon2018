package nl.ing

import nl.ing.ClassifyFood.ItemDetails
import nl.ing.MatchReceipt.ScannedReceipt
import nl.ing.model.Item
import nl.ing.model.ItemCategories.grosseries
import org.scalatest.{Matchers, WordSpecLike}


class ClassifyFoodSpec extends WordSpecLike with Matchers  {


  "the food classifier" should {

    "classify" in {
      val result = ClassifyFood.summerizeFoodGroups(model.transactions)
      println(result)
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
//      details.foreach(println)

      details.map(_.foodCategory).distinct.foreach(println)
    }

    "score name difference" in {
      val input = List(
        "AH APPEL" -> "ah appel",
        "AH APPEL" -> "ah appe",
        "AH APPEL" -> "AH Bon",
        "APPEL" -> "AH Appel")
      input.foreach(i => println(s"$i ${ClassifyFood.compareNames(i._1, i._2)}"))
    }

    "score price difference" in {
      val input = List(
        0.75F -> 1.0F,
        100F -> 0.75F,
        101F -> 100F,
        0.75F -> 0.75F)
      input.foreach(i => println(s"$i ${ClassifyFood.comparePrices(i._1, i._2)}"))
    }

    "parseAmount" in {
      val input = List("1","3"," 10","1kg","0.127kg", "300gr")
      input.foreach(i => println(s"$i ${ClassifyFood.getAmountOrGrams(i)}"))
    }
  }
}

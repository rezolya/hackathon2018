package nl.ing

import nl.ing.model.Transaction
import model._
import Math._

import nl.ing.model.ItemCategories.{grosseries, toiletries}
import FoodGroupsCategories._

object ClassifyFood {
  final case class FoodItem(item: Item, grams: Int, foodCategory: String)

  def classifyFood(transactions: List[Transaction]): FoodGroups = {
    val itemsToClassify = transactions
      .flatMap(_.items)
      .filter(_.category == ItemCategories.grosseries)

    val classified = itemsToClassify.map(i => i -> getDetails(i))

    FoodGroups()
  }

  def getDetails(item: Item): ItemDetails = {

    val similarItems = ahDB
      .filter(_.differenceScore(item) < 20)
      .map(details => (details, details.differenceScore(item)))

    val mostSimilarItem = similarItems.sortBy(_._2).headOption.getOrElse(unknownDetails -> -1)

    mostSimilarItem._1
  }

  def compareNames(detailsName: String, receiptName: String) = {
//    val
  }

  final case class ItemDetails(name: String,
                               price: Double,
                               unitSize: Int,
                               foodCategory: String) {
    def differenceScore(item: Item): Int = {
      abs(name.compareTo(item.name)) * 10 + abs(price.compareTo(item.price))
    }
  }

  val ahDB: List[ItemDetails] = List(
    ItemDetails("AH APPEL", 0.75F, 100, grosseries),
    ItemDetails("APPEL", 0.75F, 100, grosseries)
  )

  val unknownDetails = ItemDetails("unknown", 0.0, 0, unknown)
}

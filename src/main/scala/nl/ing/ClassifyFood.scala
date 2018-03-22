package nl.ing

import nl.ing.model.Transaction
import model._
import Math._

import nl.ing.model.ItemCategories.{grosseries, toiletries}
import FoodGroupsCategories._

import scala.util.{Failure, Success, Try}

object ClassifyFood {
  final case class FoodItem(item: Item, grams: Int, foodCategory: String)

  def classifyFood(transactions: List[Transaction]): FoodGroups = {
    val itemsToClassify = transactions
      .flatMap(_.items)
      .filter(_.category == ItemCategories.grosseries)

    val withDetails = itemsToClassify.map(i => i -> getDetails(i))

    val foodItems = withDetails.map{case (item, details) =>
      val grams = getAmountOrGrams(item.quantity) match {
        case Left(amount) => amount*details.unitSize
        case Right(grams) => grams
      }
      FoodItem(item, grams, details.foodCategory)
    }

    val validFoodItems = foodItems.filter(fi => fi.foodCategory!=unknown && fi.grams>0)

    val totalGrams = validFoodItems.map(_.grams).sum

    FoodGroups()
  }

  //Left(amount), Right(grams)
  def getAmountOrGrams(str: String): Either[Int, Int] = {
    Try { str.toInt } match {
      case Success(int) => Left(int)
      case Failure(_) =>
        if (str.contains("gr")) {
          val shortString = str.substring(0, str.indexOf('g'))
          Try { shortString.toInt } match {
            case Success(int) => Right(int)
            case Failure(_)   => Right(0)
          }
        } else if (str.contains("kg") || str.contains("kilo")) {
          val shortString = str.substring(0, str.indexOf('k'))
          Try { shortString.toDouble } match {
            case Success(dou) => Right((dou*1000).toInt)
            case Failure(_)   => Right(0)
          }
        } else
          Right(0)
    }
  }

  def getDetails(item: Item): ItemDetails = {

    val similarItems = ahDB
      .filter(_.differenceScore(item) < 20)
      .map(details => (details, details.differenceScore(item)))

    val mostSimilarItem =
      similarItems.sortBy(_._2).headOption.getOrElse(unknownDetails -> -1)

    mostSimilarItem._1
  }

  def compareNames(detailsName: String, receiptName: String): Int = {
    val detailsWords = detailsName.toLowerCase.split(' ')
    val receiptWords = receiptName.toLowerCase.split(' ')

    val score =
      detailsWords.zip(receiptWords).count { case (d, r) => !d.startsWith(r) }
    score
  }
  def comparePrices(detailsPrice: Float, receiptPrice: Float): Int = {
    val total = detailsPrice + receiptPrice

    val detailsPerc = 100 * detailsPrice / total
    val receiptPerc = 100 * receiptPrice / total
    abs(detailsPerc - receiptPerc).toInt
  }

  final case class ItemDetails(name: String,
                               price: Double,
                               unitSize: Int,
                               foodCategory: String) {
    def differenceScore(item: Item): Int = {
      compareNames(name, item.name) * 10 + comparePrices(price.toFloat,
                                                         item.price)
    }
  }

  val ahDB: List[ItemDetails] = List(
    ItemDetails("AH APPEL", 0.75F, 100, fruit),
    ItemDetails("APPEL", 0.75F, 100, fruit),
    ItemDetails("Kipfilet", 5F, 100, meat)
  )

  val unknownDetails = ItemDetails("unknown", 0.0, 0, unknown)
}

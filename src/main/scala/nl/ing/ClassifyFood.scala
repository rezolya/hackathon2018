package nl.ing

import nl.ing.model.Transaction
import model._
import Math._
import java.io.InputStream

import nl.ing.model.ItemCategories.{grosseries, toiletries}
import FoodGroupsCategories._

import scala.util.{Failure, Success, Try}

import scala.io.Source

object ClassifyFood {

  final case class FoodItem(item: Item, grams: Int, foodCategory: String)

  def classifyFood(transactions: List[Transaction]): FoodGroups = {
    val itemsToClassify = transactions
        .flatMap(_.items)
        .filter(_.category == ItemCategories.grosseries)

    val withDetails = itemsToClassify.map(i => i -> getDetails(i))

    val foodItems = withDetails.map {
      case (item, details) =>
        val grams = getAmountOrGrams(item.quantity) match {
          case Left(amount) => amount * details.unitSize
          case Right(grams) => grams
        }
        FoodItem(item, grams, details.foodCategory)
    }

    val validFoodItems =
      foodItems.filter(fi => fi.foodCategory != unknown && fi.grams > 0)

    val totalGrams = validFoodItems.map(_.grams).sum

    val vegetableSum = 100 * getCategorySum(validFoodItems, vegetable) / totalGrams
    val fruitSum = 100 * getCategorySum(validFoodItems, fruit) / totalGrams
    val breadSum = 100 * getCategorySum(validFoodItems, bread) / totalGrams
    val grainSum = 100 * getCategorySum(validFoodItems, grain) / totalGrams
    val meatSum = 100 * getCategorySum(validFoodItems, meat) / totalGrams
    val nutsSum = 100 * getCategorySum(validFoodItems, nuts) / totalGrams
    val dairySum = 100 * getCategorySum(validFoodItems, dairy) / totalGrams
    val cheeseSum = 100 * getCategorySum(validFoodItems, cheese) / totalGrams
    val fatsSum = 100 * getCategorySum(validFoodItems, fats) / totalGrams
    val notAdvisedSum = 100 * getCategorySum(validFoodItems, notAdvised) / totalGrams

    FoodGroups(vegetableSum,
      fruitSum,
      breadSum,
      grainSum,
      meatSum,
      nutsSum,
      dairySum,
      cheeseSum,
      fatsSum,
      notAdvisedSum)
  }

  def getCategorySum(list: List[FoodItem], category: String): Int = {
    list.filter(_.foodCategory == category).map(_.grams).sum
  }

  //Left(amount), Right(grams)
  def getAmountOrGrams(str: String): Either[Int, Int] = {
    Try {
      str.toInt
    } match {
      case Success(int) => Left(int)
      case Failure(_) =>
        if (str.contains("gr")) {
          val shortString = str.substring(0, str.indexOf('g'))
          Try {
            shortString.toInt
          } match {
            case Success(int) => Right(int)
            case Failure(_) => Right(0)
          }
        } else if (str.contains("kg") || str.contains("kilo")) {
          val shortString = str.substring(0, str.indexOf('k'))
          Try {
            shortString.toDouble
          } match {
            case Success(dou) => Right((dou * 1000).toInt)
            case Failure(_) => Right(0)
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
                               foodCategory: String,
                               category: String = ItemCategories.grosseries) {
    def differenceScore(item: Item): Int = {
      compareNames(name, item.name) * 10 + comparePrices(price.toFloat,
        item.price.toFloat)
    }
  }

  object ItemDetails {
    def loadFrom(fileName: String): Seq[ItemDetails] = {
      val stream: InputStream = getClass.getResourceAsStream(fileName)
      Source
          .fromInputStream(stream)
          .getLines()
          .flatMap { line =>
            val strings: Array[String] = line.split('\t')
            strings match {
              case Array(name, category, price, unit) =>
                val foodCategory = category.replace("\u00AD", "").intern() match {
                  case s
                    if s.startsWith(
                      "Aardappel, groente, fruit".replace("\u00AD", "")) =>
                    vegetable
                  case s
                    if s.startsWith("Vlees, kip, vis,".replace("\u00AD", "")) =>
                    meat
                  case s
                    if s.startsWith("Vlees, kip, vis,".replace("\u00AD", "")) =>
                    meat
                  case s if s.contains("Brood".replace("\u00AD", "")) => bread
                  case s
                    if s.startsWith("Pasta, rijst, ".replace("\u00AD", "")) =>
                    grain
                  case s if s.contains("Zuivel".replace("\u00AD", "")) => dairy
                  case s
                    if s.startsWith("Zuivel, eieren ".replace("\u00AD", "")) =>
                    dairy
                  case s
                    if s.startsWith(
                      "Kaas, vleeswaren, delicatessen".replace("\u00AD", "")) =>
                    cheese
                  case s
                    if s.startsWith(
                      "Verse kant-en-klaar maaltijden".replace("\u00AD", "")) =>
                    notAdvised
                  case s
                    if s.startsWith(
                      "Soepen, conserven, sauzen,".replace("\u00AD", "")) =>
                    notAdvised
                  case s if s.contains("Snoep".replace("\u00AD", "")) =>
                    notAdvised
                  case s => s
                }

                Some(
                  ItemDetails(name.replace("\u00AD", "").intern(),
                    price.toDouble,
                    convertUnit(unit),
                    foodCategory))
              case _ =>
                None
            }
          }
          .toSeq
    }

    private val patGram = """([\d\.]+)\s+g""".r
    private val patMilliliter = """([\d\.]+)\s+ml""".r
    private val patKilogram = """([\d\.]+)\s+kg""".r
    private val patLiter = """([\d\.]+)\s+l""".r
    private val patNrLiters = """([\d\.]+)\s*x\s*([\d.]+)\s+l""".r
    private val patStuks = """([\d\.]+)\s+stuks""".r
    private val patPerStuk = """per stuk""".r

    private def convertUnit(unit: String) = {
      val convUnit = unit.replace(',', '.') match {
        case patNrLiters(i, u) => i.toInt * u.toDouble * 1000
        case patGram(u) => u.toDouble
        case patMilliliter(u) => u.toDouble
        case patKilogram(u) => u.toDouble * 1000
        case patLiter(u) => u.toDouble * 1000
        case patStuks(u) => u.toDouble * 100
        case patPerStuk(u) => u.toDouble * 100
        case _ => 1
      }
      convUnit.toInt
    }
  }

  val ahDB: List[ItemDetails] = Seq(
    "/AH-products/aardappel-groente-fruit.csv",
    "/AH-products/bakkerij.csv",
    "/AH-products/diepvries.csv",
    "/AH-products/frisdrank-sappen-koffie-thee.csv",
    "/AH-products/kaas-vleeswaren-delicatessen.csv",
    "/AH-products/soepen-conserven-sauzen-smaakmakers.csv",
    "/AH-products/verse-kant-en-klaar-maaltijden-salades.csv",
    "/AH-products/vlees-kip-vis-vega.csv",
    "/AH-products/zuivel-eieren.csv"
  ).flatMap {
    ItemDetails.loadFrom(_)
  }.toList

  val unknownDetails = ItemDetails("unknown", 0.0, 0, unknown)
}

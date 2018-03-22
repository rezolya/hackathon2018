package nl.ing

import nl.ing.model.Transaction
import model._
import Math._
import java.io.InputStream

import nl.ing.model.ItemCategories.{grosseries, toiletries}
import FoodGroupsCategories._

import scala.io.Source

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

    val mostSimilarItem =
      similarItems.sortBy(_._2).headOption.getOrElse(unknownDetails -> -1)

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

  object ItemDetails {
    def loadFrom(fileName: String): Seq[ItemDetails] = {
      val stream: InputStream = getClass.getResourceAsStream(fileName)
      Source.fromInputStream(stream).getLines().flatMap { line =>
        val strings: Array[String] = line.split('\t')
        strings match {
          case Array(name, category, price, unit) =>
            Some(ItemDetails(name.intern(), price.toDouble, convertUnit(unit), category.intern()))
          case _ =>
            None
        }
      }.toSeq
    }

    private val patGram ="""([\d\.]+)\s+g""".r
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
    "/AH-products/zuivel-eieren.csv").flatMap {
    ItemDetails.loadFrom(_)
  }.toList

  val unknownDetails = ItemDetails("unknown", 0.0, 0, unknown)
}

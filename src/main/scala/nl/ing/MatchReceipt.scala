package nl.ing

import nl.ing.model._
import Math._

object MatchReceipt {

  final case class ScannedReceipt(items: List[Item], total: Double, shopName: String)

  def catagorizeReceiptItems(items: List[Item]): List[Item] = {
    items.map { x =>
      model.someItems.find(si => si.name.equals(x.name)) match {
        case Some(luItem) => x.copy(category = luItem.category)
        case None => x.copy(category = ItemCategories.unspecified)
      }
    }

  }

  def matchReceipt(scannedReceipt: ScannedReceipt): Unit = {
    transactions.find{x =>
      abs(x.amount) == scannedReceipt.total
    } match {
      case Some(transaction) =>
        val items: List[model.Item] = catagorizeReceiptItems(scannedReceipt.items)
        val categoriesWeights: Map[String, Int] = items
          .groupBy(_.category).mapValues(i => math.abs((i.map(_.price).sum * 100) / transaction.amount).toInt)
        val categories = Categories(
          categoriesWeights.getOrElse(ItemCategories.grosseries, 0),
          categoriesWeights.getOrElse(ItemCategories.toiletries, 0),
          categoriesWeights.getOrElse(ItemCategories.restaurants, 0),
          categoriesWeights.getOrElse(ItemCategories.clothes, 0),
          categoriesWeights.getOrElse(ItemCategories.furniture, 0),
          categoriesWeights.getOrElse(ItemCategories.toys, 0)
        )

        updateTransactions(items, categories, transaction)
      case None =>
    }
  }

  private def updateTransactions(items: List[Item],
                                 categories: Categories,
                                 transaction: Transaction) = {
    transactions = transactions.map { tr =>
      if (tr == transaction)
        transaction.copy(items = items, categories = categories)
      else tr
    }
  }
}

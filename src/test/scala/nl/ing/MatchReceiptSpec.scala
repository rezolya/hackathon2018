package nl.ing

import nl.ing.MatchReceipt.ScannedReceipt
import nl.ing.model.{Categories, Item}
import nl.ing.model.ItemCategories.{grosseries, toiletries}
import org.scalatest.{Inside, Matchers, WordSpecLike}

class MatchReceiptSpec extends WordSpecLike with Matchers with Inside {
  "Matching a receipt" in {
    MatchReceipt.matchReceipt(ScannedReceipt(List(
      Item("APPEL", 0.75, "1", ""),
      Item("LABELLO", 1.75, "1", ""),
      Item("OET PIZZA", 5.3, "2", ""),
      Item("YAKITORI", 3.59, "1", ""),
      Item("DR PEPPER", 0.69, "1", "")
    ), -12.08))
    inside(model.transactions.find(_.amount == -12.08)) {
      case Some(transaction) =>
        transaction.items.map(_.category) shouldBe Seq(grosseries,toiletries,grosseries,grosseries,grosseries)
        transaction.categories shouldBe Categories(85,14,0,0,0,0)
    }
  }
}

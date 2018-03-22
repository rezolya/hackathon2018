package nl.ing

import nl.ing.model._

class MatchReceipt {

  final case class ScannedReceipt(items: List[Item], total: Double)

//  def matchReceipt(scannedReceipt: ScannedReceipt): Receipt = {
//    transactions.find(_.amount) match {
//      case Some(transaction) =>
//        updateTransactions(scannedReceipt.items, transaction)
////        Receipt(transaction.benificiary.name, scannedReceipt.total, scannedReceipt.items, )
//???
//      case None => Receipt("", 0F, List.empty, Categories())
//    }
//  }

  private def updateTransactions(items: List[Item],
                                 transaction: Transaction) = {
    transactions = transactions.map { tr =>
      if (tr == transaction)
        transaction.copy(items = items)
      else tr
    }
  }
}

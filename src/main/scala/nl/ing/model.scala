package nl.ing

import java.time.ZonedDateTime

import nl.ing.model.ItemCategories.toiletries
import spray.json.DefaultJsonProtocol._

import scala.concurrent.{ExecutionContext, Future}

object model {

  var receipts: List[Seq[Byte]] = List.empty
  import ItemCategories._

  val transactions: List[Transaction] = List(
    Transaction(
      Account("Etos", "1234"),
      -12.05,
      ZonedDateTime
        .parse("2018-03-21T10:30:00.00+01:00")
        .toInstant
        .toEpochMilli,
      List(Item("Labello", 1.75F, toiletries),
           Item("Shampoo", 3.3F, toiletries),
           Item("Shower gel", 7F, toiletries)),
      Categories(toiletries = 100)
    ),
    Transaction(Account("AH Togo", "5678"),
                -32.05,
                ZonedDateTime
                  .parse("2018-03-21T10:30:00.00+01:00")
                  .toInstant
                  .toEpochMilli),
    Transaction(
      Account("Yari", "5678"),
      -89.99,
      ZonedDateTime
        .parse("2018-03-21T10:30:00.00+01:00")
        .toInstant
        .toEpochMilli,
      List(Item("Shoes", 80.00F, clothes), Item("Scarf", 9.99F, clothes)),
      Categories(clothes = 100)
    ),
    Transaction(Account("HurryUp", "5678"),
                -32.05,
                ZonedDateTime
                  .parse("2018-03-21T10:30:00.00+01:00")
                  .toInstant
                  .toEpochMilli),
    Transaction(Account("Bristol", "5678"),
                -32.05,
                ZonedDateTime
                  .parse("2018-03-21T10:30:00.00+01:00")
                  .toInstant
                  .toEpochMilli),
    Transaction(Account("NS", "5678"),
                -32.05,
                ZonedDateTime
                  .parse("2018-03-21T10:30:00.00+01:00")
                  .toInstant
                  .toEpochMilli),
    Transaction(Account("Q-Park", "5678"),
                -32.05,
                ZonedDateTime
                  .parse("2018-03-21T10:30:00.00+01:00")
                  .toInstant
                  .toEpochMilli)
  ).sortBy(x => x.timestamp)

  // domain model
  final case class Item(name: String, price: Float, category: String = "")

  final case class Receipt(shopName: String,
                           totalTransactionAmount: Float,
                           items: List[Item],
                           categories: Categories)

  final case class Categories(grosseries: Int = 0,
                              toiletries: Int = 0,
                              restaurants: Int = 0,
                              clothes: Int = 0,
                              furniture: Int = 0,
                              toys: Int = 0)

  final case class Account(name: String, number: String)

  final case class Transaction(benificiary: Account,
                               amount: Double,
                               timestamp: Long,
                               items: List[Item] = List.empty,
                               categories: Categories = Categories())

  final case class AccountOverview(account: Account,
                                   transactions: List[Transaction])

  // formats for unmarshalling and marshalling
  implicit val itemFormat = jsonFormat3(Item)
  implicit val categoriesFormat = jsonFormat6(Categories)
  implicit val orderFormat = jsonFormat4(Receipt)
  implicit val accountFormat = jsonFormat2(Account)
  implicit val transactionFormat = jsonFormat5(Transaction)
  implicit val accountOverviewFormat = jsonFormat2(AccountOverview)

  def fetchTransactions(offset: Int, size: Int)(
      implicit executionContext: ExecutionContext): Future[AccountOverview] =
    Future {
      AccountOverview(Account("Olga", "1234566"),
                      transactions.slice(offset, offset + size))
    }

  object ItemCategories {
    val grosseries = "grosseries"
    val toiletries = "toiletries"
    val restaurants = "restaurants"
    val clothes = "clothes"
    val furniture = "furniture"
    val toys = "toys"
  }
}

package nl.ing

import java.time.ZonedDateTime

import nl.ing.MatchReceipt.ScannedReceipt
import spray.json.DefaultJsonProtocol._

import scala.concurrent.{ExecutionContext, Future}

object model {

  var receipts: List[Seq[Byte]] = List.empty
  import ItemCategories._

  val someItems = List(
    Item("AH APPEL", 0.75F, "1", grosseries),
    Item("APPEL", 0.75F, "1", grosseries),
    Item("LABELLO", 1.75F, "1", toiletries),
    Item("OET PIZZA", 5.3F, "2", grosseries),
    Item("YAKITORI", 3.59F, "1", grosseries),
    Item("DR PEPPER", 0.69F, "1", grosseries),
    Item("FUET SPANJE", 2.29F, "1", grosseries),
    Item("VARKENSFILET", 2.98F, "0.298kg", grosseries),
    Item("RED BULL", 1.25F, "1", grosseries),
    Item("PASTASALADE", 9.38F, "2", grosseries),
    Item("AH BOLLEN", 1F, "1", grosseries),
    Item("KIPFILET", 2.6F, "0.127kg", grosseries)
  )

  var transactions: List[Transaction] = List(
    Transaction(
      Account("Etos", "1234"),
      -12.05,
      ZonedDateTime
        .parse("2018-03-21T10:30:00.00+01:00")
        .toInstant
        .toEpochMilli,
      List(Item("Labello", 1.75, "1", toiletries),
           Item("Shampoo", 3.3, "1", toiletries),
           Item("Shower gel", 7, "2", toiletries)),
      Categories(toiletries = 100)
    ),
    Transaction(
      Account("AH Togo", "5678"),
      -12.98,
      ZonedDateTime
        .parse("2018-03-21T10:30:00.00+01:00")
        .toInstant
        .toEpochMilli,
      List.empty
    ),
    Transaction(
      Account("AH Togo", "5678"),
      -1.5,
      ZonedDateTime
        .parse("2018-03-21T10:30:00.00+01:00")
        .toInstant
        .toEpochMilli,
      List.empty
    ),
    Transaction(
      Account("Yari", "5678"),
      -89.99,
      ZonedDateTime
        .parse("2018-03-21T10:30:00.00+01:00")
        .toInstant
        .toEpochMilli,
      List(Item("Shoes", 80.00, "1", clothes),
           Item("Scarf", 9.99, "1", clothes)),
      Categories(clothes = 100)
    ),
    Transaction(Account("HurryUp", "5678"),
                -32.05,
                ZonedDateTime
                  .parse("2018-03-21T10:30:00.00+01:00")
                  .toInstant
                  .toEpochMilli),    Transaction(
      Account("Albert Hein", "5678"),
      -19.5,
      ZonedDateTime
          .parse("2018-03-21T10:30:00.00+01:00")
          .toInstant
          .toEpochMilli,
      List(
        Item("FUET SPANJE", 2.29, "1", grosseries),
        Item("VARKENSFILET", 2.98, "0.298kg", grosseries),
        Item("RED BULL", 1.25, "1", grosseries),
        Item("PASTASALADE", 9.38, "2", grosseries),
        Item("AH BOLLEN", 1, "1", grosseries),
        Item("KIPFILET", 2.6, "0.127kg", grosseries)
      ),
      Categories(grosseries = 100)
    ),
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
  final case class Item(name: String,
                        price: Double,
                        quantity: String,
                        category: String = "")

  final case class Receipt(shopName: String,
                           totalTransactionAmount: Float,
                           items: Seq[Item],
                           categories: Categories)

  final case class Categories(grosseries: Int = 0,
                              toiletries: Int = 0,
                              restaurants: Int = 0,
                              clothes: Int = 0,
                              furniture: Int = 0,
                              toys: Int = 0)

  final case class FoodGroups(vegetable: Double = 0,
                              fruit: Double = 0,
                              bread: Double = 0,
                              grain: Double = 0,
                              meat: Double = 0,
                              nuts: Double = 0,
                              dairy: Double = 0,
                              cheese: Double = 0,
                              fats: Double = 0,
                              notAdvised: Double = 0)

  final case class FoodGroupsResult(actual: FoodGroups,
                                    advised: FoodGroups = advisedFoodGroups)

  val advisedFoodGroups = FoodGroups(
    vegetable = 16.39,
    fruit = 13.11,
    bread = 9.84,
    grain = 19.67,
    meat = 7.87,
    nuts = 1.64,
    dairy = 26.23,
    cheese = 2.62,
    fats = 2.62
  )

  final case class Account(name: String, number: String)

  final case class Transaction(benificiary: Account,
                               amount: Double,
                               timestamp: Long,
                               items: List[Item] = List.empty,
                               categories: Categories = Categories())

  final case class AccountOverview(account: Account,
                                   transactions: List[Transaction])

  // formats for unmarshalling and marshalling
  implicit val itemFormat = jsonFormat4(Item)
  implicit val categoriesFormat = jsonFormat6(Categories)
  implicit val orderFormat = jsonFormat4(Receipt)
  implicit val accountFormat = jsonFormat2(Account)
  implicit val transactionFormat = jsonFormat5(Transaction)
  implicit val accountOverviewFormat = jsonFormat2(AccountOverview)
  implicit val foodGroupsFormat = jsonFormat10(FoodGroups)
  implicit val foodGroupsResultFormat = jsonFormat2(FoodGroupsResult)
  implicit val scannedReceiptFormat = jsonFormat3(ScannedReceipt)

  def fetchTransactions(offset: Int, size: Int)(
      implicit executionContext: ExecutionContext): Future[AccountOverview] =
    Future {
      AccountOverview(Account("Mw OA Someone", "NL99 1234 5678 90"),
                      transactions.slice(offset, offset + size))
    }

  def fetchFoodGroupsResult(
      implicit executionContext: ExecutionContext): Future[FoodGroupsResult] =
    Future {
      val actualFoodGroups = FoodGroups(
        fruit = 7F,
        bread = 9F,
        meat = 19F,
        notAdvised = 65F
      )
      FoodGroupsResult(actualFoodGroups)
    }

  object ItemCategories {
    val grosseries = "grosseries"
    val toiletries = "toiletries"
    val restaurants = "restaurants"
    val clothes = "clothes"
    val furniture = "furniture"
    val toys = "toys"
    val unspecified = "unspecified"
  }

  object FoodGroupsCategories {
    val vegetable = "vegetable"
    val fruit = "fruit"
    val bread = "bread"
    val grain = "grain"
    val meat = "meat"
    val nuts = "nuts"
    val dairy = "dairy"
    val cheese = "cheese"
    val fats = "fats"
    val notAdvised = "notAdvised"
    val unknown = "unknown"
  }
}

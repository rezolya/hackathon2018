package nl.ing

import java.time.ZonedDateTime

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
      List(Item("Labello", 1.75F, "1", toiletries),
           Item("Shampoo", 3.3F, "1", toiletries),
           Item("Shower gel", 7F, "2", toiletries)),
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
      List(Item("Shoes", 80.00F, "1", clothes),
           Item("Scarf", 9.99F, "1", clothes)),
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
  final case class Item(name: String,
                        price: Float,
                        quantity: String,
                        category: String = "")

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

  def fetchTransactions(offset: Int, size: Int)(
      implicit executionContext: ExecutionContext): Future[AccountOverview] =
    Future {
      AccountOverview(Account("Olga", "1234566"),
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
  }
}

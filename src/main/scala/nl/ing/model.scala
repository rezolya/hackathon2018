package nl.ing

import java.time.{ZoneId, ZonedDateTime}
import java.time.format.DateTimeFormatter

import akka.Done
import spray.json.{DefaultJsonProtocol, JsString, JsValue, RootJsonFormat}
import spray.json.DefaultJsonProtocol._

import scala.concurrent.{ExecutionContext, Future}

object model {

  var receipts: List[Seq[Byte]] = List.empty

  var orders: List[Item] = List(
    Item("brood", 1)
  )

  val transactions: List[Transaction] = List(
    Transaction(Account("Etos", "1234"),
      -12.05,
      ZonedDateTime.parse("2018-03-21T10:30:00.00+01:00").toInstant.toEpochMilli),
    Transaction(Account("AH Togo", "5678"),
      -32.05,
      ZonedDateTime.parse("2018-03-21T10:30:00.00+01:00").toInstant.toEpochMilli),
    Transaction(Account("Yari", "5678"),
      -32.05,
      ZonedDateTime.parse("2018-03-21T10:30:00.00+01:00").toInstant.toEpochMilli),
    Transaction(Account("HurryUp", "5678"),
      -32.05,
      ZonedDateTime.parse("2018-03-21T10:30:00.00+01:00").toInstant.toEpochMilli),
    Transaction(Account("Bristol", "5678"),
      -32.05,
      ZonedDateTime.parse("2018-03-21T10:30:00.00+01:00").toInstant.toEpochMilli),
    Transaction(Account("NS", "5678"),
      -32.05,
      ZonedDateTime.parse("2018-03-21T10:30:00.00+01:00").toInstant.toEpochMilli),
    Transaction(Account("Q-Park", "5678"),
      -32.05,
      ZonedDateTime.parse("2018-03-21T10:30:00.00+01:00").toInstant.toEpochMilli)
  ).sortBy(x => x.timestamp)

  // domain model
  final case class Item(name: String, id: Long)

  final case class Order(items: List[Item])

  final case class Account(name: String, number: String)

  final case class Transaction(benificiary: Account,
                               amount: Double,
                               timestamp: Long)

  final case class AccountOverview(account: Account,
                                   transactions: List[Transaction])

  // formats for unmarshalling and marshalling
  implicit val itemFormat = jsonFormat2(Item)
  implicit val orderFormat = jsonFormat1(Order)
  implicit val accountFormat = jsonFormat2(Account)
  implicit val transactionFormat = jsonFormat3(Transaction)
  implicit val accountOverviewFormat = jsonFormat2(AccountOverview)

  // (fake) async database query api
  def fetchItem(itemId: Long)(
    implicit executionContext: ExecutionContext): Future[Option[Item]] =
    Future {
      orders.find(o => o.id == itemId)
    }

  def saveOrder(order: Order)(
    implicit executionContext: ExecutionContext): Future[Done] = {
    orders = order match {
      case Order(items) => items ::: orders
      case _ => orders
    }
    Future {
      Done
    }
  }

  def fetchTransactions(offset: Int, size: Int)(
    implicit executionContext: ExecutionContext): Future[AccountOverview] =
    Future {
      AccountOverview(Account("Olga", "1234566"), transactions.slice(offset, offset + size))
    }
}

package nl.ing

import java.time.ZonedDateTime

import spray.json.DefaultJsonProtocol._

import scala.concurrent.{ExecutionContext, Future}

object model {

  var receipts: List[Seq[Byte]] = List.empty

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
  final case class Item(name: String, price: Float)

  final case class Receipt(shopName: String, totalTransactionAmount: Float, items: List[Item])

  final case class Account(name: String, number: String)

  final case class Transaction(benificiary: Account,
                               amount: Double,
                               timestamp: Long)

  final case class AccountOverview(account: Account,
                                   transactions: List[Transaction])

  // formats for unmarshalling and marshalling
  implicit val itemFormat = jsonFormat2(Item)
  implicit val orderFormat = jsonFormat3(Receipt)
  implicit val accountFormat = jsonFormat2(Account)
  implicit val transactionFormat = jsonFormat3(Transaction)
  implicit val accountOverviewFormat = jsonFormat2(AccountOverview)

  def fetchTransactions(offset: Int, size: Int)(
    implicit executionContext: ExecutionContext): Future[AccountOverview] =
    Future {
      AccountOverview(Account("Olga", "1234566"), transactions.slice(offset, offset + size))
    }
}

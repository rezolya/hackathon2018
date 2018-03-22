package nl.ing

import akka.actor.ActorSystem
import akka.http.javadsl.model.headers.RawHeader
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.model.{headers, _}
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Sink
import akka.util.ByteString
import nl.TextRecognition2
import nl.ing.model._

import scala.io.StdIn

object WebServer {

  def main(args: Array[String]) {

    implicit val system = ActorSystem("my-system")
    implicit val materializer = ActorMaterializer()
    // needed for the future flatMap/onComplete in the end
    implicit val executionContext = system.dispatcher

    val route =
      respondWithHeaders(
        headers.RawHeader("Access-Control-Allow-Origin", "*"),
        headers.RawHeader("Access-Control-Allow-Methods", "Get, POST"),
        headers.RawHeader("Access-Control-Allow-Headers",
                          "Origin, X-Requested-With, Content-Type, Accept")
      ) {
        get {
          path("hello") {
            complete(HttpEntity(ContentTypes.`text/html(UTF-8)`,
                                "<h1>Say hello to akka-http</h1>"))
          }
        } ~
          //upload receipt endpoint.
          path("uploadReceipt") {

            fileUpload("receipt") {
              case (metadata, byteSource) =>
                //                val sink = FileIO.toPath(Paths.get("/tmp") resolve metadata.fileName)
                val sink = Sink.fold[ByteString, ByteString](ByteString.empty) {
                  case (acc: ByteString, chunk: ByteString) => acc ++ chunk
                }
                val writeResult = byteSource.runWith(sink)
                onSuccess(writeResult) { result =>
                  val listOfBytes = result.toList

                  TextRecognition2.detectDocumentText(listOfBytes)

                  receipts = receipts :+ listOfBytes

                  val stubbedReceipt =
                    Receipt(
                      "AHTogo",
                      19.99F,
                      List(
                        Item("BonBons", 10.00, "1", ItemCategories.grosseries),
                        Item("Appels", 5.00, "1", ItemCategories.grosseries),
                        Item("Pepermunt ballen",
                             4.99,
                             "1",
                             ItemCategories.grosseries)
                      ),
                      Categories(grosseries = 90, toiletries = 10)
                    )

                  /*{"shopName":"AHTogo","totalTransactionAmount":19.989999771118164,
                  "items":[{"name":"BonBons","price":10.0},{"name":"Appels","price":5.0,"category":"grocery"},{"name":"Pepermunt ballen","price":4.98,"category":"grocery"}]
                  "category": {"grocery" : 10, "toiletries": 90}
                }*/
                  complete(stubbedReceipt)
                }
            }
          } ~
          //get transactions endpoint.
          pathPrefix("getTransactions" / IntNumber / IntNumber) {
            case (offset, size) =>
              val eventualAccountOverview = fetchTransactions(offset, size)
              onSuccess(eventualAccountOverview) { accountOverview =>
                complete(accountOverview)
              }
          } ~
          pathPrefix("getFoodGroups") {
            val eventualResult = fetchFoodGroupsResult
            onSuccess(eventualResult) { result =>
              complete(result)
            }
          }
      }

    val bindingFuture = Http().bindAndHandle(route, "localhost", 8080)

    println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
    StdIn.readLine() // let it run until user presses return
    bindingFuture
      .flatMap(_.unbind()) // trigger unbinding from the port
      .onComplete(_ => system.terminate()) // and shutdown when done
  }
}

package nl.ing

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.model.{headers, _}
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Sink
import akka.util.ByteString
import nl.ReceiptRecognition
import nl.ing.model._

import scala.concurrent.Future
import scala.io.StdIn
import scala.util.Success

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

                  val googleResponse: Future[Success[MatchReceipt.ScannedReceipt]] = ReceiptRecognition.detectDocumentText(listOfBytes)
                  onSuccess(googleResponse){
                    case Success(receipt) =>
                      MatchReceipt.matchReceipt(receipt)
                      println(transactions)
                      complete(receipt)
                  }
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

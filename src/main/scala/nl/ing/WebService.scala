package nl.ing

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.model._
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
      get {
        path("hello") {
          complete(
            HttpEntity(ContentTypes.`text/html(UTF-8)`,
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

                complete(s"Successfully written ${listOfBytes.size} bytes")
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
        }

    val bindingFuture = Http().bindAndHandle(route, "localhost", 8080)

    println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
    StdIn.readLine() // let it run until user presses return
    bindingFuture
      .flatMap(_.unbind()) // trigger unbinding from the port
      .onComplete(_ => system.terminate()) // and shutdown when done
  }
}

package nl

import java.util.Base64

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.Materializer
import nl.ing.MatchReceipt.ScannedReceipt
import nl.ing.receiptLocations.{GoogleAnswerJsonParser, ItemsExtractor, Schema}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

object ReceiptRecognition {

  val apiKey = "AIzaSyCweUb0_GyA8WQ2vIH3_pXPNgxBSyGJGvo"

//  case class Image(content: String)
//  case class Feature(`type`: String)
//  case class OCRRequest(image: Image, features: Feature = Feature( "TEXT_DETECTION"))
//
//  implicit val ocrRequestFormat = jsonFormat2(OCRRequest)

//  def getRequest(imageBytes: Seq[Byte]) = {
//    val base64Image = Try(Base64.getEncoder.encodeToString(imageBytes.toArray))
//
//    OCRRequest(Image(base64Image.get))
//  }

  def detectDocumentText(imageBytes: Seq[Byte])(
      implicit actorSystem: ActorSystem,
      executionContext: ExecutionContext,
      materializer: Materializer): Future[Success[ScannedReceipt]] = {
    val base64Image = Try(Base64.getEncoder.encodeToString(imageBytes.toArray))

    val json = s"""{
                 |  "requests": [
                 |    {
                 |      "image": {
                 |        "content": "${base64Image.get}"
                 |      },
                 |      "features": [
                 |        {
                 |          "type": "TEXT_DETECTION"
                 |        }
                 |      ]
                 |    }
                 |  ]
                 |}""".stripMargin

    val responseFuture: Future[HttpResponse] = Http().singleRequest(
      HttpRequest(
        uri = s"https://vision.googleapis.com/v1/images:annotate?key=$apiKey",
        method = HttpMethods.POST,
        entity = HttpEntity.apply(ContentTypes.`application/json`, json)
      ))

    responseFuture
        .flatMap{
      //.onComplete {
        case res =>
//          println(res)
          val eventualString: Future[String] = Unmarshal(res.entity).to[String]
          eventualString.map(_ match {
            case jsonString =>
              val parsedSchema: Schema = GoogleAnswerJsonParser.parse(jsonString)
              Success(new ItemsExtractor(parsedSchema).getReceipt)
          })
      }
  }
}

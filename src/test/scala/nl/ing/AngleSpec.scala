package nl.ing

import java.io.InputStream

import nl.ing.receiptLocations.{Item, Rectangle, Schema}
import org.scalatest.{Matchers, WordSpecLike}
import spray.json.JsValue

import scala.collection.immutable
import scala.io.Source

class AngleSpec extends WordSpecLike with Matchers  {

  case class Coordinate(x: Int, y: Int)


  "the angle" should {

    "be correctly determined" in {
      val LL =Coordinate(0,0)
      val LR =Coordinate(1,-1)
      val xDifference: Float = LR.x - LL.x
      val yDifference: Float = LR.y - LL.y
      val helling = Math.atan(yDifference / xDifference)
      println(helling)
    }

    "do some shit" in {

      def trimQuotes(input: String): String =
        input.replaceAll("\"", "")

      import spray.json._

      val stream = getClass.getResourceAsStream("/Example.json")
      val json = Source.fromInputStream(stream).mkString

      val jsonObject = json.asJson.asJsObject
      val jsonTextAnnotations = jsonObject.getFields("responses").head.asInstanceOf[JsArray].elements.head.asJsObject
      val usefullJsonArray = jsonTextAnnotations.getFields("textAnnotations").head.asInstanceOf[JsArray].elements
      val items : Seq[Item] = usefullJsonArray.map(jsonValue => {
        val innerJsonObject = jsonValue.asJsObject()
        val name = trimQuotes(innerJsonObject.getFields("description").head.toString())
        val vertices = innerJsonObject.getFields("boundingPoly").head.asJsObject.getFields("vertices").head.asInstanceOf[JsArray]
        val corners = vertices.elements.map(element => {
          val x = element.asJsObject.getFields("x").head.asInstanceOf[JsNumber].value.toInt
          val y = element.asJsObject.getFields("y").head.asInstanceOf[JsNumber].value.toInt
          nl.ing.receiptLocations.Vec(x, y)
        })
        val rectangle = Rectangle(corners(0), corners(1), corners(2), corners(3))
        Item(name, rectangle)
      }).tail

      val lines = Schema(items).getLines.map(line => line.map(item => item.name))
      lines.foreach( line => println(line.mkString(" : ")))
    }

    "blub" in {
    }
  }
}

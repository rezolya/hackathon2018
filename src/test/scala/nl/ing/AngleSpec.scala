package nl.ing

import java.io.InputStream

import nl.ing.receiptLocations.{GoogleAnswerJsonParser, Item, Rectangle, Schema}
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

    "parse a ticket" in {

      val stream = getClass.getResourceAsStream("/Example.json")
      val json = Source.fromInputStream(stream).mkString

      val schema = GoogleAnswerJsonParser.parse(json)
      val lines = schema.getLines.map(line => line.map(item => item.name))
      lines.foreach( line => println(line.mkString(" : ")))
    }

    "line up these two particular lines" in {
      val stream = getClass.getResourceAsStream("/Example.json")
      val json = Source.fromInputStream(stream).mkString

      val schema = GoogleAnswerJsonParser.parse(json)
      val itemSubtotaal = schema.items.filter(item => item.name == "SUBTOTAA").head
      val item150 = schema.items.filter(_.name == "1,50").head
      itemSubtotaal.coordinate.isOnSameHeight(item150.coordinate)
    }
  }
}

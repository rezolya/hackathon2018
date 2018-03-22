package nl.ing.receiptLocations

import scala.io.Source

object GoogleAnswerJsonParser {

  def parse(json: String): Schema = {
    import spray.json._
    def trimQuotes(input: String): String =
      input.replaceAll("\"", "")

    val jsonObject = json.asJson.asJsObject
    val jsonTextAnnotations = jsonObject.getFields("responses").head.asInstanceOf[JsArray].elements.head.asJsObject
    val usefullJsonArray = jsonTextAnnotations.getFields("textAnnotations").head.asInstanceOf[JsArray].elements
    val items = usefullJsonArray.map(jsonValue => {
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
    Schema(items)
  }
 // {
//    import spray.json._
//    val jsonObject = json.asJson.asJsObject
//    val usefullJsonArray: immutable.Seq[JsValue] = jsonObject.getFields("responses").head.asJsObject().getFields("textAnnotations")
//    val items = usefullJsonArray.map(jsonValue => {
//      val jsonObject = jsonValue.asJsObject()
//      val name = jsonObject.getFields("description").head.toString()
//      val vertices = jsonObject.getFields("boundingPoly").head.asJsObject.getFields("vertices")
//    })
//    Schema(items)
//  }

}

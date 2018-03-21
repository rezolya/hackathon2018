package nl.ing.receiptLocations

case class Vector(x: Double, y: Double)

case class Line(slope: Double, height: Double) {
  def intersection(otherLine: Line): Option[Vector] = {
    if(otherLine.slope == slope) None
    else {
      val x = (otherLine.height - height)/(slope - otherLine.slope)
      val y = slope * x + height
      Some(Vector(x, y))
    }
  }
}

case class Rectangle(bottomLeft: Vector, bottomRight: Vector, upperRight: Vector, upperLeft: Vector) {

  lazy val middle: Vector = ???

  def isOnSameHeight(otherSquare: Rectangle): Boolean = ???
}
case class Item(name: String, coordinate: Rectangle)

case class Schema(item : Seq[Item])
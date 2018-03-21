package nl.ing.receiptLocations

case class Vector(x: Double, y: Double)

case class Line(slope: Double, hight: Double) {
  def intersection(otherLine: Line): Option[Vector] = {

  }
}

case class Square(topRight: Vector, topLeft: Vector, bottomRight: Vector, bottomLeft: Vector) {

  val middle: Vector = ???

  def isOnSameHeight(otherSquare: Square): Boolean = ???
}
case class Item(name: String, coordinate: Square)

case class Schema(item : Seq[Item])
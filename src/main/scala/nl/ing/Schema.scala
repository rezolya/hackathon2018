package nl.ing


case class Vector(x: Int, y: Int)
case class Square(topRight: Vector, topLeft: Vector, bottomRight: Vector, bottomLeft: Vector) {

  val middle: Vector = ???

  def isOnSameLine(otherSquare: Square): Boolean = ???
}
case class Item(name: String, coordinate: Square)

case class Schema(item : Seq[Item])
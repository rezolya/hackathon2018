package nl.ing.receiptLocations

object Vec {
  def apply(x: Int, y: Int): Vec = {
    Vec(x.toDouble, y.toDouble)
  }
}

case class Vec(x: Double, y: Double)

object Line {

  def getSlope(point1: Vec, point2: Vec): Option[Double] = {
    if(point1.x == point2.x) None
    else if(point1.x < point2.x) {
      val xDifference = point2.x - point1.x
      val yDifference = point2.y - point1.y
      Some(yDifference / xDifference)
    } else {
      val xDifference = point1.x - point2.x
      val yDifference = point1.y - point2.y
      Some(yDifference / xDifference)
    }
  }

  def getLineThroughPoint(point: Vec, slope: Double): Line = {
    val height = point.y - (slope * point.x)
    Line(slope, height)
  }

  def getLineThroughPoints(point1: Vec, point2: Vec): Option[Line] = {
    getSlope(point1, point2) match {
      case None =>
        None
      case Some(slope) =>
        val height = point1.y - (slope * point1.x)
        Some(Line(slope, height))
    }
  }
}

case class Line(slope: Double, height: Double) {
  def intersection(otherLine: Line): Option[Vec] = {
    if(otherLine.slope == slope) None
    else {
      val x = (otherLine.height - height)/(slope - otherLine.slope)
      val y = slope * x + height
      Some(Vec(x, y))
    }
  }
}

case class Rectangle(bottomLeft: Vec, bottomRight: Vec, upperRight: Vec, upperLeft: Vec) {

  import Line._
  lazy val middle: Vec = getLineThroughPoints(bottomLeft, upperRight).get.intersection(getLineThroughPoints(bottomRight, upperLeft).get).get

  def isToTheLeftOf (otherSquare: Rectangle): Boolean = {
    Math.max(bottomRight.x, upperRight.x) <= Math.min(otherSquare.bottomLeft.x, otherSquare.upperLeft.x)
  }

  def isOnSameHeight(otherSquare: Rectangle): Boolean = {
    (isToTheLeftOf(otherSquare), otherSquare.isToTheLeftOf(this)) match {
      case (true, false) =>
        val xUpper = otherSquare.upperLeft.x
        val xBottom = otherSquare.bottomLeft.x
        if(xUpper == xBottom) {
          val lineThroughMiddle = getLineThroughPoint(middle, getSlope(bottomLeft, bottomRight).get)
          val height = lineThroughMiddle.slope * xUpper + lineThroughMiddle.slope

          otherSquare.bottomLeft.y <= height && height <= otherSquare.upperLeft.y
        } else {

          val lineThroughMiddle = getLineThroughPoint(middle, getSlope(bottomLeft, bottomRight).get)
          val lineThroughLeftSideOfOtherRectangle = getLineThroughPoints(otherSquare.bottomLeft, otherSquare.upperLeft)
          val intersection = lineThroughMiddle.intersection(lineThroughLeftSideOfOtherRectangle.get).get

          if(xUpper < xBottom) {
            xUpper < intersection.x && intersection.x < xBottom
          } else {
            xBottom < intersection.x && intersection.x < xUpper
          }
        }
      case (false, true) =>
        otherSquare.isOnSameHeight(this)
      case (_, _) =>
        false
    }
  }
}
case class Item(name: String, coordinate: Rectangle)

case class Schema(item : Seq[Item])
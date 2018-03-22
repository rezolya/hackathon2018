package nl.ing.receiptLocations

import scala.annotation.tailrec

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

  lazy val length: Double = bottomRight.x - bottomLeft.x

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
        val upperLine = getLineThroughPoints(upperLeft, upperRight).get
        val bottomLine = getLineThroughPoints(bottomLeft, bottomRight).get

        if(xUpper == xBottom) {
          val lineThroughMiddle = getLineThroughPoint(middle, getSlope(bottomLeft, bottomRight).get)

          Seq(upperLine, bottomLine, lineThroughMiddle).exists(line => {
            val height = line.slope * xUpper + line.slope

            otherSquare.bottomLeft.y <= height && height <= otherSquare.upperLeft.y

          })
        } else {

          val lineThroughMiddle = getLineThroughPoint(middle, getSlope(bottomLeft, bottomRight).get)
          val lineThroughLeftSideOfOtherRectangle = getLineThroughPoints(otherSquare.bottomLeft, otherSquare.upperLeft)

          Seq(upperLine, bottomLine, lineThroughMiddle).exists(line => {
            val intersection = line.intersection(lineThroughLeftSideOfOtherRectangle.get).get

            if(xUpper < xBottom) {
              xUpper <= intersection.x && intersection.x <= xBottom
            } else {
              xBottom <= intersection.x && intersection.x <= xUpper
            }
          })
        }
      case (false, true) =>
        otherSquare.isOnSameHeight(this)
      case (_, _) =>
        false
    }
  }
}
case class Item(name: String, coordinate: Rectangle)

case class Schema(items : Seq[Item]) {

  object ItemWidthOrdering extends Ordering[Item] {
    override def compare(x: Item, y: Item): Int = x.coordinate.length compare y.coordinate.length
  }

  private def getBiggestOfTheFirstFive(items: Seq[Item]): (Item, Seq[Item]) = {
    val subList = items.slice(0, Math.max(5, items.size))
    val maxItem = subList.max(ItemWidthOrdering)
    (maxItem, items.filterNot(_.equals(maxItem)))
  }

  def getLines: Seq[Seq[Item]] = {

    def innergetLines(itemList: Seq[Item]): Seq[Seq[Item]] = {
      itemList.toSeq match {
        case Nil =>
          Seq.empty
        case seq: Seq[Item] =>
          val (maxItem, rest): (Item, Seq[Item]) = getBiggestOfTheFirstFive(seq)
          val (itemsOnSameLine, remaining): (Seq[Item], Seq[Item]) = rest.partition(item => maxItem.coordinate.isOnSameHeight(item.coordinate))
          //val (itemsOnSameLine, remaining) = tail.partition(item => head.coordinate.isOnSameHeight(item.coordinate))
          Seq(Seq(maxItem) ++ itemsOnSameLine) ++ innergetLines(remaining)
        case vector: Vector[Item] =>
          val (maxItem, rest): (Item, Seq[Item]) = getBiggestOfTheFirstFive(vector)
          val (itemsOnSameLine, remaining): (Seq[Item], Seq[Item]) = rest.partition(item => maxItem.coordinate.isOnSameHeight(item.coordinate))
          //val (itemsOnSameLine, remaining) = tail.partition(item => head.coordinate.isOnSameHeight(item.coordinate))
          Seq(Seq(maxItem) ++ itemsOnSameLine) ++ innergetLines(remaining)

      }
    }

    innergetLines(items)
  }
}
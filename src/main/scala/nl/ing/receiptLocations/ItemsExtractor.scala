package nl.ing.receiptLocations

import nl.ing.model.{Categories, Item, Receipt}

class ItemsExtractor(schema: Schema) {
  val lines = schema.getLines.map(_.map(_.name))
  val amountRegex = """(\d+)[,\.](\d\d)""".r

  def isAnAmount(input: String): Boolean = {
    input match {
      case amountRegex(_, _) =>
        true
      case _ =>
        false
    }
  }

  type stringChecker = Seq[String] => Boolean


  private def getAllSubstrings(string: String): Seq[String] = string.inits.flatMap(_.tails.toList.init).toList
  private def isSameAs(a: String, b: String): Boolean = a.toLowerCase equals b.toLowerCase
  private def contains(superString: String, subString: String): Boolean = getAllSubstrings(superString).exists(sub => isSameAs(sub, subString))

  def getReceipt: Receipt = {
    val shopName = getShopName

    val totalAmount = getTotalAmount
    val itemsBought = getItems()

    Receipt(shopName, totalAmount, itemsBought.map(boughtItem => Item(getItemName(boughtItem), getLargestAmount(boughtItem), "1", "")), new Categories())
  }

  private def getItemName(item: Seq[String]): String = {
    item.filterNot(value => isAnAmount(value)).mkString(" ")
  }

  private def getItems() = {
    val possibleItemLines = getLineNumberOfSubtotal
    possibleItemLines.filter(line => {
      line.lengthCompare(2) > 0}).filter(_.exists(isAnAmount(_)))
  }

  object amountOrdering extends Ordering[(Int, Int)] {
    override def compare(x: (Int, Int), y: (Int, Int)): Int = if((x._1 compare y._1) == 0) x._2 compare x._2
    else x._1 compare x._1
  }

  private def getLargestAmount(item: Seq[String]): String = {
    val amountInIntPairs: Seq[(Int, Int)] = item.flatMap {
      case amountRegex(euro, cents) =>
        Some((euro.toString.toInt, cents.toString.toInt))
      case _ =>
        None
    }
    val minAmount = amountInIntPairs.min(amountOrdering)
    s"${minAmount._1},${minAmount._2}"

  }

  private def getLineNumberOfSubtotal = {
    val totalLineNumber: Int = lines.zipWithIndex.filter(line => line._1.exists(item => contains(item, "subtotaa"))).head._2
    val omschrijvingLineNumber: Int = lines.zipWithIndex.filter(line => line._1.exists(item => contains(item, "omschrijving"))).head._2
    lines.slice(omschrijvingLineNumber, totalLineNumber)
  }

  private def getTotalAmount = {
    val linesWithTotalAmount: Seq[Seq[String]] = lines.filter(line => line.exists(item => contains(item, "subtotaa")))
    linesWithTotalAmount.filter(_.exists(isAnAmount(_))).head.filter(isAnAmount(_)).head
  }

  private def getShopName = {
    val firstFourLinesFlattened = lines.slice(0,3).flatten
    if(checkForAH(firstFourLinesFlattened)) "Albert Heijn"
    else if(checkForCoop(firstFourLinesFlattened)) "Coop"
    else if(checkForIntertoys(firstFourLinesFlattened)) "Intertoys"
    else if(checkForIntratuin(firstFourLinesFlattened)) "Intratuin"
    else "Unknown"
  }


  private val checkForCoop: stringChecker = inputStrings => inputStrings.exists(inputString => {
    contains(inputString, "coop")
  })

  private val checkForIntertoys: stringChecker = inputStrings => inputStrings.exists(inputString => {
    contains(inputString, "intertoys")
  })

  private val checkForIntratuin: stringChecker = inputStrings => inputStrings.exists(inputString => {
    contains(inputString, "intratuin")
  })

  private val checkForAH: stringChecker = {
    inputStrings => {
      val nameVariants = Seq("ah", "albert", "heijn")
      nameVariants.exists(nameToCheck => {
        inputStrings.exists(inputString => contains(inputString, nameToCheck))
      })
    }
  }
}

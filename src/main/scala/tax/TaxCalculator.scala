package tax

import java.nio.charset.CodingErrorAction
import java.text.DecimalFormat
import java.util.Date
import scala.annotation.tailrec
import scala.io.Codec

object TaxCalculator extends App {
  implicit val codec: Codec = Codec("UTF-16")
  codec.onMalformedInput(CodingErrorAction.REPLACE)
  codec.onUnmappableCharacter(CodingErrorAction.REPLACE)

  val zero = BigDecimal(0.0)

  // read exante .csv
  val source = io.Source.fromResource("Custom_CNV8068.001.csv")
  // read usd rates .csv

  // parse .csv
  // "Transaction ID"	"Account ID"	"Symbol ID"	"ISIN"	"Operation type"	"When"	"Sum"	"Asset"	"EUR equivalent"	"Comment"
  private val data: Seq[Array[String]] = (for {
    // drop header
    line <- source.getLines().drop(31)
    //_ = println(line)
    columns = line.split("\t").map(s => s.stripPrefix("\"").stripSuffix("\""))
  } yield columns ).toSeq
  source.close()

  val entries: Seq[Entry] = data.map(Transaction(_))

  def trade(entries: Seq[Entry]) = {
    val Some(cash) = entries.find(e => e.operation == "TRADE" && e.asset == "USD")
    val Some(commission) = entries.find(e => e.operation == "COMMISSION")
    val stock = (entries.toSet - cash - commission).head
    if (stock.qty > 0.0)
      Seq(Purchase(stock.symbol, stock.date, -(cash.qty + commission.qty), stock.qty.toInt))
    else
      Seq(Sale(stock.symbol, stock.date, cash.qty + commission.qty, -stock.qty.toInt))
  }

  // group entries to transactions by time, ticker, price. aggregate sums
  val transactions: Iterable[Transaction] = entries.groupBy(_.date).flatMap {
    // interests
    case (_, Seq(e)) =>
      //if (e.operation == "INTEREST") println("Dropped [INTEREST]") else println(s"Dropped [OTHER]: $e")
      Seq()
    case (_, entries) if entries.size == 2 => entries.sortBy(_.operation == "TAX") match {
      // dividend and tax
      case Seq(dividend, tax) if tax.operation == "TAX" =>
        Seq(Dividend(dividend.symbol, dividend.date, dividend.qty, 1, -tax.qty))
      // or currency exchange
      case _ =>
        println(s"Dropped [OTHER]: $entries")
        Seq()
    }
    // trades
    case (_, entries) if entries.size == 3 => trade(entries)
    case (_, entries) => entries.sliding(3, 3).flatMap(trade)
  }.toSeq.sortBy(t => t.date)

  //transactions.foreach(println(_))

  val purchases = transactions.groupBy(_.stock).map { case (stock, transactions) =>
    val purchases = transactions
      .filter(_.isInstanceOf[Purchase])
      .map(_.asInstanceOf[Purchase])
      .toSeq.sortBy(_.date)
      .foldLeft(Seq.empty[Purchase]) { case (acc, p) =>
        val totalQty = acc.lastOption.map(_.totalQty).getOrElse(0) + p.qty
        val totalAmount = acc.lastOption.map(_.totalAmount).getOrElse(zero) + p.price
        acc :+ p.copy(totalQty = totalQty, totalAmount = totalAmount)
      }
    (stock, purchases)
  }
  val sales = transactions.groupBy(_.stock).map { case (stock, transactions) =>
    val sales = transactions
      .filter(_.isInstanceOf[Sale])
      .map(_.asInstanceOf[Sale])
      .toSeq.sortBy(_.date)
      .foldLeft(Seq.empty[Sale]) { case (acc, p) =>
        val totalQty = acc.lastOption.map(_.totalQty).getOrElse(0) + p.qty
        val totalAmount = acc.lastOption.map(_.totalAmount).getOrElse(zero) + p.price
        acc :+ p.copy(totalQty = totalQty, totalAmount = totalAmount)
      }
    (stock, sales)
  }
  val dividends = transactions.filter(_.isInstanceOf[Dividend]).map(_.asInstanceOf[Dividend])

  purchases
    //.filter(_._1 == "HII.NYSE")
    .flatMap(_._2).foreach(p => println(p.toExcel))
  println("----------------------------------------------------")
  sales
    //.filter(_._1 == "HII.NYSE")
    .flatMap(_._2).foreach(s => println(s.toExcel))
  println("----------------------------------------------------")

  def calcProfits(purchases: Seq[Purchase], sales: Seq[Sale]): Seq[Profit] = {
    // find purchases in range (sale.totalQty - sale.qty) <= purchase.totalQty <= sale.totalQty
    // balance = purchase.totalQty - (sale.totalQty - sale.qty)
    // profit.qty = balance min sale.qty; profit.cost = purchase.price; profit.revenue = sale.price
    // repeat balance = purchase.qty
    @tailrec
    def go(sale: Sale)(purchases: Seq[Purchase], balance: Int, profits: Seq[Profit]): Seq[Profit] =
      if (purchases.isEmpty) profits else {
        val purchase = purchases.head
        val newBalance = balance + purchase.qty
        val qty = newBalance min sale.qty

        def profit = if (qty > 0) {
          val cost = purchase.price / purchase.qty
          val revenue = sale.price / sale.qty
          Some(Profit(sale.stock, purchase.date, sale.date, qty, cost, revenue))
        } else None

        go(sale)(purchases.tail, newBalance - qty, profits ++ profit.toSeq)
      }

    def purchasesInRange(from: Double, to: Double) = {
      // drop while total is less or equal then from
      // then take while total is less or equal then to
      purchases.dropWhile(_.totalQty <= from).span(_.totalQty <= to) match { case (h, t) => h ++ t.take(1)}
    }

    def lastOpt(range: Seq[Purchase]) = range.headOption.toSeq

    for {
      sale <- sales
      soldBefore = sale.totalQty - sale.qty
      range = purchasesInRange(soldBefore, sale.totalQty)
      purchase <- lastOpt(range)
      balance = purchase.totalQty - purchase.qty - soldBefore
      profit <- go(sale)(range, balance, Seq())
    } yield profit
  }


  val profits = purchases.map { case (stock, purchases) => calcProfits(purchases, sales(stock)) }
  profits.flatten.toSeq
    //.filter(_.stock == "HII.NYSE")
    .sortBy(_.sellDate)
    .foreach(p => println(p.toExcel))
    //.foreach(p => println(p.decimalFormat.format(p.profit)))

  val capitalGain = profits.flatten.map(_.profit).sum
  val totalDividendsAmount = dividends.map(_.price).sum
  val totalDividendTaxesAmount = dividends.map(_.tax).sum
  println(s"\ntotal transactions: ${transactions.size}, " +
    s"dividends total:${totalDividendsAmount + totalDividendTaxesAmount}, " +
    s"gross: $totalDividendsAmount, tax: $totalDividendTaxesAmount, " +
    s"capital gain: $capitalGain")
}

case class Entry(symbol: String, asset: String, operation: String, date: Date, qty: BigDecimal)

sealed trait Transaction {
  def stock: String
  def date: Date
  def price: BigDecimal
  def qty: Int
  def pricePerUnit: BigDecimal = price / qty
  def toExcel: String = s"${getClass.getName} $stock\t${date}\t$qty\t$pricePerUnit\t$price"
}

object Transaction {
  val readDateformat = new java.text.SimpleDateFormat("yyyy-MM-dd hh:mm:ss")

  def apply(array: Array[String]): Entry =
    (array(2), array(7), array(4), readDateformat.parse(array(5)), array(6).toDouble) match {
    case (symbol, asset, operation, date, sum) => Entry(symbol, asset, operation, date, sum)
  }

}

case class Purchase(stock: String, date: Date, price: BigDecimal, qty: Int,
                    totalQty: Int = 0, totalAmount: BigDecimal = 0.0) extends Transaction {
  def tax: Double = 0.0
}

case class Sale(stock: String, date: Date, price: BigDecimal, qty: Int,
                totalQty: Int = 0, totalAmount: BigDecimal = 0.0) extends Transaction {
  def tax: Double = 0.0
}

case class Dividend(stock: String, date: Date, price: BigDecimal, qty: Int, tax: BigDecimal) extends Transaction

case class Profit(stock: String, purchaseDate: Date, sellDate: Date, qty: Int, purchasePrice: BigDecimal, sellPrice: BigDecimal) {
  val decimalFormat = {
    import java.text.DecimalFormatSymbols
    val symbols = new DecimalFormatSymbols()
    symbols.setDecimalSeparator(',')
    symbols.setGroupingSeparator('.')
    new DecimalFormat("###.###", symbols)
  }
  val dataFormat = new java.text.SimpleDateFormat("dd.MM.yyyy hh:mm:ss")

  def profit: BigDecimal = (sellPrice - purchasePrice) * qty

  def toExcel: String =
    s"$stock\t" +
      s"${dataFormat.format(purchaseDate)}\t" +
      s"${dataFormat.format(sellDate)}\t" +
      s"$qty\t" +
      s"${decimalFormat.format(purchasePrice)}\t" +
      s"${decimalFormat.format(sellPrice)}\t" +
      s"${decimalFormat.format(profit)}"
}

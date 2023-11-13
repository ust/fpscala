package report

import investment.Profit
import parser.model._
import util.{ExanteParser, FileDataReader}

import scala.annotation.tailrec

object ExanteProfitReport extends App {
  val zero = BigDecimal(0.0)

  val entries: Seq[Entry] = (new FileDataReader with ExanteParser)("Custom_CNV8068.001.csv").map(Transaction(_))

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
      purchases.dropWhile(_.totalQty <= from).span(_.totalQty <= to) match {
        case (h, t) => h ++ t.take(1)
      }
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

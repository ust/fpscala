package investment

import java.text.DecimalFormat
import java.util.Date

case class Account(id: String, owner: String)
case class Portfolio(holdings: Seq[Security])

trait Security { def ticker: String; def name: String }
case class Bond(ticker: String, name: String) extends Security
case class Stock(ticker: String, name: String) extends Security

case class Trade(security: Security, amount: BigDecimal, price: BigDecimal, time: Date)
abstract class Fee(val amount: BigDecimal, val date: Date)
case class Dividend(stock: Stock, amount: BigDecimal, date: Date)
abstract class Interest(desc: String, amount: BigDecimal, date: Date)
case class TradeFee(override val amount: BigDecimal, override val date: Date) extends Fee(amount, date)
abstract class Tax(amount: BigDecimal)


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

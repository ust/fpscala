package investment

import java.text.DecimalFormat
import java.util.Date

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

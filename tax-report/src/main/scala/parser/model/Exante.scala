package parser.model

import java.util.Date

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

  def apply(fields: Seq[String]): Entry =
    (fields(2), fields(7), fields(4), readDateformat.parse(fields(5)), fields(6).toDouble) match {
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

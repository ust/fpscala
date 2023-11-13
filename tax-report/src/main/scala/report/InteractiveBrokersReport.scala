package report

import investment.{Stock, Trade}
import util.{ExanteParser, FileDataReader, InteractiveBrokersParser}

object InteractiveBrokersReport extends App {
  //2023-03-27, 13:28:15
  val format = new java.text.SimpleDateFormat("\"yyyy-MM-dd HH:mm:ss\"")

  private val reader = new FileDataReader with InteractiveBrokersParser
  // read files and tokenize
  val data = reader("U4312539_20230102_20230825.csv") ++
    reader("U4312539_2022_2022.csv") ++
    reader("U4312539_2021_2021.csv") ++
    reader("U4312539_2020_2020.csv")

  // pass table to parser
  //Trades,Header,DataDiscriminator,Asset Category,Currency,Symbol,Date/Time,Quantity,T. Price,C. Price,Proceeds,Comm/Fee,Basis,Realized P/L,MTM P/L,Code
  val records = data.filter(_.take(2) == Seq("Trades", "Data"))
    .filter(_(3) != "Forex") // skip currency exchanges
    .filter(_.size == 17) // penny stock issue: KSHB has 1000+ shares and "," split thousands

  // read broker specific data
  val trades = records.map { record =>
    val security = if (record(3) == "Stocks") Stock(record(5), "") else ???
    Trade(security, BigDecimal(record(8)), BigDecimal(record(9)), format.parse(record(6) + record(7)))
  } foreach println
  // build domain model

  // print capital gains
  records.foreach { array =>
    println(array.mkString(","))
  }
  // print dividends
  // print interests
}

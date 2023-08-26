package util

import java.nio.charset.CodingErrorAction
import scala.io.Codec

class FileDataReader {
  implicit val codec: Codec = Codec("UTF-16")
  codec.onMalformedInput(CodingErrorAction.REPLACE)
  codec.onUnmappableCharacter(CodingErrorAction.REPLACE)

  def apply(fileName: String): Seq[Array[String]] = {

    // read exante .csv
    val source = io.Source.fromResource(fileName)
    // read usd rates .csv
    // parse .csv
    // "Transaction ID"	"Account ID"	"Symbol ID"	"ISIN"	"Operation type"	"When"	"Sum"	"Asset"	"EUR equivalent"	"Comment"
    val data = (for {
      // drop header
      line <- source.getLines().drop(31)
      //_ = println(line)
      columns = line.split("\t").map(s => s.stripPrefix("\"").stripSuffix("\""))
    } yield columns).toVector

    source.close()

    data
  }
}

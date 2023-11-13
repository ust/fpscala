package util

import java.nio.charset.CodingErrorAction
import scala.io.{BufferedSource, Codec}

abstract class FileDataReader {
  implicit val codec: Codec = {
    val codec = Codec("UTF-8")
    codec.onMalformedInput(CodingErrorAction.REPLACE)
    codec.onUnmappableCharacter(CodingErrorAction.REPLACE)
    codec
  }

  def tokenize(source: BufferedSource): Seq[Seq[String]]

  def apply(fileName: String): Seq[Seq[String]] = {
    // read exante .csv
    val source = io.Source.fromResource(fileName)
    // read usd rates .csv
    // parse .csv
    val data = tokenize(source)

    source.close()

    data
  }
}

trait ExanteParser { self: FileDataReader =>
  override val codec: Codec = {
    val codec = Codec("UTF-16")
    codec.onMalformedInput(CodingErrorAction.REPLACE)
    codec.onUnmappableCharacter(CodingErrorAction.REPLACE)
    codec
  }

  def tokenize(source: BufferedSource): Seq[Seq[String]] = {
    // "Transaction ID"	"Account ID"	"Symbol ID"	"ISIN"	"Operation type"	"When"	"Sum"	"Asset"	"EUR equivalent"	"Comment"
    val data = (for {
      // drop header
      line <- source.getLines().drop(31)
      //_ = println(line)
      columns = line.split("\t").map(s => s.stripPrefix("\"").stripSuffix("\"")).toSeq
    } yield columns).toVector
    data
  }
}

trait InteractiveBrokersParser { self: FileDataReader =>
  def tokenize(source: BufferedSource): Seq[Seq[String]] = {
    val data = (for {
      // drop header
      line <- source.getLines()
      //_ = println(line)
      columns = line.split(",").toSeq
    } yield columns).toVector
    data
  }
}

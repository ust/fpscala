
object JustRunner {
  implicit val StringDecoder: Decoder[String] = identity
  implicit val DoubleDecoder: Decoder[Double] = (str: String) => str.toDouble

  implicit val StringEncoder: Encoder[String] = identity
  implicit val DoubleEncoder: Encoder[Double] = (value: Double) => value.toString

  def main(args: Array[String]): Unit = {
    val k1 = Key[String]("k1")
    val k2 = Key[Double]("k2")
    val datastore = new Datastore

    datastore.set(k1)("hey")
    datastore.set(k2)(.223)
    assert {
      datastore.get(k2).contains(.223)
    }
  }
}

case class Key[T](name: String)

trait Decoder[T] { def decode(str: String): T }

trait Encoder[T] { def encode(value: T): String }

class Datastore {
  val db = scala.collection.mutable.Map.empty[String, String]

  def set[T](key: Key[T])(value: T)(implicit encoder: Encoder[T]): Unit = db.update(key.name, encoder.encode(value))

  def get[T](key: Key[T])(implicit decoder: Decoder[T]): Option[T] = db.get(key.name).map(decoder.decode)
}
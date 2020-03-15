package here

import com.vividsolutions.jts.geom._
import com.vividsolutions.jts.triangulate.DelaunayTriangulationBuilder

import collection.JavaConverters._

case class Geometries(factory: GeometryFactory) {
  def point(value: Coordinate): Point = factory.createPoint(value)

  def polygon(coords: Seq[Coordinate]): Polygon =
    factory.createPolygon((coords :+ coords.head).toArray)
}

object Geometries {
  def coord(x: Double, y: Double, z: Double): Coordinate = new Coordinate(x, y, z)
}

object Triangulation extends App {
  val f = Geometries(new GeometryFactory())
  import Geometries._


  def triangulate(coordinates: Seq[Coordinate]): Geometry = {

    val b = new DelaunayTriangulationBuilder

    b.setSites(coordinates.asJavaCollection)

    val g = b.getTriangles(f.factory)

    g
  }

  {
//    val g = triangulate(Seq(
//      coord(1, 1, 1), coord(2, 1, 1), coord(2, 2, 1)
//    ))


    val t = f.polygon(Seq(coord(0, 0, 0), coord(5, 0, 0), coord(5, 5, 5), coord(0, 5, 5)))
    val (p1, p2) = (f.point(coord(1, 1, 1)), f.point(coord(1, 1, 1.5)))
//    println( g covers p1 ) // fixme g is collection geometry
    println( t covers p2 )
  }


}

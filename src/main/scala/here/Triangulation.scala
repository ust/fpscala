package here

import com.vividsolutions.jts.geom.{Coordinate, Geometry, GeometryFactory, Point}
import com.vividsolutions.jts.triangulate.DelaunayTriangulationBuilder

import collection.JavaConverters._

case class Geometries(factory: GeometryFactory) {
  def point(value: Coordinate): Point = factory.createPoint(value)
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
    val g = triangulate(Seq(
      coord(1, 1, 1), coord(2, 1, 1), coord(2, 2, 1)
    ))

    val p = f.point(coord(1, 1, 1))
    println( g covers p ) // fixme g is collection geometry
  }


}

package ucl.scenario.geometry

import CommonUtils._
import PointUtils._
import SegmentUtils._

import scala.collection.mutable

/**
  * A generic implementation of a polygon, implemented as a list of its own
  * vertices (ordered counter-clockwise). Therefore, it also admits
  * definition of degenerated polygons.
  *
  * @author Ilya Sergey
  *
  */

case class Polygon(vertices: Seq[Point2D]) extends EpsEq[Polygon] {

  def getVertices: List[Point2D] = vertices.toList

  override def toString = {
    vertices.map(_.toString).mkString(", ")
  }

  def =~=(q: Polygon): Boolean = {
    val vs = q.vertices
    if (vs.size != vertices.size) return false

    // Check the rotations of vertice sequence
    for (i <- 0 to vs.size) {
      if (seqEpsEq(vertices, rotateSeqNum(vs, i))) return true
    }

    false
  }

  def edges: Seq[Segment] =
    PolygonUtils.getEdges(vertices).map { case (a, b) => Segment(a, b) }

  def rotateCWAndShift(phi: Double, newOrigin: Point2D): Polygon = {
    assert(vertices.size >= 3)
    val tmpOrigin = vertices.head
    val shiftedToOrigin = vertices.map(_ - tmpOrigin)
    // rotate clockwise
    val rotated = shiftedToOrigin.map(_.rotateClockWise(phi))
    val shiftedToDest = rotated.map(_ + newOrigin)
    // Remove artifacts of rotation
    val res = Polygon(shiftedToDest).prettifyAlmostIntVertices
    res
  }

  def stretch(k: Double): Polygon = {
    assert(k > 0)
    Polygon(vertices.map(p => Point2D(p.x * k, p.y * k)))
  }

  def prettifyAlmostIntVertices = Polygon(vertices.map(roundPoint))

  def containsPoint(p: Point2D) = PolygonUtils.containsPoint(this, p)

  /**
    * @return true is this polygon is convex
    */
  def isConvex = PolygonUtils.checkConvex(vertices)

  /**
    * Move polygon pol so x would become its origin
    */
  def shiftToOrigin(x: Point2D) = Polygon(vertices.map(v => v - x))

  /**
    * Remove aligned vertices from a polygon boundary and return a new polygon
    */
  def removeAligned: Polygon = {
    val n = vertices.size
    val triplesInd =
      (for (k <- 0 to n - 3) yield (k, k + 1, k + 2)) ++
          Seq((n - 2, n - 1, 0), (n - 1, 0, 1))
    val triplesV = triplesInd.map { case (i, i1, i2) => (vertices(i), vertices(i1), vertices(i2)) }

    val fs = triplesV.filter { case (a, b, c) => crossProduct(a, b, c) =!= 0.0 }
    Polygon(fs.map(_._2))
  }

}

/**
  * Util functions for polygons
  */
object PolygonUtils {

  // [WTF] Somehow more type-safe structural subtyping doesn't work here, so I jsut use AnyVal
  implicit def _points2Poly(ps: Seq[(AnyVal, AnyVal)]): Polygon = {

    // Crude but efficient and simple
    val ps1 = ps.map { case (a, b) =>
      (a.asInstanceOf[ToDouble].toDouble, b.asInstanceOf[ToDouble].toDouble)
    }
    Polygon(ps1.map { case (a, b) => Point2D(a, b) })
  }

  /**
    * Checking convexity of a polygon
    */
  def checkConvex(p: Polygon): Boolean = checkConvex(p.vertices)

  def checkConvex(vs: Seq[Point2D]): Boolean = {
    // Checking by implementing a z-crossproduct algorithm

    if (vs.size <= 2) return true

    def pos(f: Double) = f > 0

    val n = vs.size
    val triplesInd =
      (for (k <- 0 to n - 3) yield (k, k + 1, k + 2)) ++
          Seq((n - 2, n - 1, 0), (n - 1, 0, 1))

    val triplesV = triplesInd.map { case (i, i1, i2) => (vs(i), vs(i1), vs(i2)) }
    val zps = triplesV.map { case (a, b, c) => crossProduct(a, b, c) }

    val allPos = zps.forall(pos)
    val allNeg = zps.forall(x => !pos(x))

    allPos || allNeg
  }

  /**
    * Check if a point is inside of a polygon
    */
  def containsPoint(pn: Polygon, pt: Point2D): Boolean = {
    val vs = pn.vertices

    // Trivial cases
    if (vs.size == 1) return vs.head =~= pt
    if (vs.size == 2) return pointOnSegment(pt, (vs.head, vs(1)))

    // Check the boundary
    for (e <- pn.edges) {
      if (pointOnSegment(pt, e)) {
        return true
      }
    }

    // Check the interior
    containsPointProperly(pn.vertices, pt)
  }

  def containsPointProperly(vs: Seq[Point2D], p: Point2D): Boolean = {

    if (vs.size <= 2) return false

    // Proper containment
    var counter = 0
    var p1 = vs.head
    val ps = vs.tail ++ Seq(p1)
    for (p2 <- ps) {
      if (p.y >~ Math.min(p1.y, p2.y) &&
          p.y <=~ Math.max(p1.y, p2.y) &&
          p.x <=~ Math.max(p1.x, p2.x) &&
          p1.y != p2.y) {
        val xinters = (p.y - p1.y) * (p2.x - p1.x) / (p2.y - p1.y) + p1.x
        if ((p1.x =~= p2.x) || p.x <=~ xinters) {
          counter = counter + 1
        }
      }
      p1 = p2
    }

    /**
      * odd  number of intersections = inside
      * even number of intersections = outside
      */
    !(counter % 2 == 0)
  }

  def getEdges[T](vs: Seq[T]): Seq[(T, T)] =
    if (vs.size <= 1) Nil
    else {
      val n = vs.size
      (for (i <- 1 until n) yield (vs(i - 1), vs(i))) ++ Seq((vs(n - 1), vs.head))
    }

  private def shrinkUniformly(p: Polygon, k: Double): Polygon = {
    assert(k > 0)
    val vs = for (v <- p.vertices) yield Point2D(v.x / k, v.y / k)
    Polygon(vs).prettifyAlmostIntVertices
  }

  def rotateToEdge(p: Polygon, e: Segment): Polygon = {
    // e is really an edge of p
    assert(p.edges.contains(e))
    // shift to e.b
    // rotate so former [a, b] would be on a negative axis and b would be (0, 0)
    val vs = p.shiftToOrigin(e.b).vertices.map(_.rotateClockWise(e.angle).toCart)
    // make (0, 0) to be the first one
    val rt = rotateSeq(vs, origin2D)._1
    Polygon(rt).prettifyAlmostIntVertices
  }

  def edgeIsConvex(p: Polygon, e: Segment): Boolean = {
    val rotated = rotateToEdge(p, e)
    for (v <- rotated.vertices) {
      if (v.y <~ 0.0) return false
    }
    true
  }

  def findShortestConvexEdge(p: Polygon): Option[Segment] = {
    val convexE = p.edges.filter(edgeIsConvex(p, _))
    if (convexE.isEmpty) return None
    val h = convexE.head
    val t = convexE.tail
    // Find the shortest edge
    val res = t.foldLeft(h)((r, e) => if (r.length <= e.length) r else e)
    Some(res)
  }

  def prepareForAttachment(p: Polygon, vertK: Double): Option[Polygon] = {
    val er = findShortestConvexEdge(p)
    if (er.isEmpty) return None
    val e = er.get
    val k = e.length
    // Make the attachment edge 1
    val p1 = shrinkUniformly(rotateToEdge(p, e), k)
    // Stretch vertically
    //val vs = for (v <- p1.vertices) yield Point2D(v.x, v.y * k)
    val vs = p1.vertices
    Some(Polygon(vs))
  }

}

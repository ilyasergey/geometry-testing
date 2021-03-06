package ucl.scenario.geometry

import org.scalatest.{MustMatchers, FlatSpec, Matchers}
import PointUtils._
import PolygonUtils._
import SegmentUtils._
import ucl.scenario.geometry.examples.UsefulPolygons
import UsefulPolygons._

/**
  * @author ilya
  */

class PolyTests extends FlatSpec with Matchers {


  s"A triangle $trianglePoly" should "be identified as convex polygon" in {
    assert(trianglePoly.isConvex)
  }

  s"A convex $convexPoly" should "be identified as such" in {
    assert(convexPoly.isConvex)
  }

  s"A convex $convexPoly2" should "be identified as such" in {
    assert(convexPoly2.isConvex)
  }

  s"A convex $convexPoly3" should "be identified as such" in {
    assert(convexPoly3.isConvex)
  }

  s"A non-convex $simpleNonConvexPoly" should "be identified as such" in {
    assert(!simpleNonConvexPoly.isConvex)
  }

  s"A non-convex $nonConvexPoly5" should "be identified as such" in {
    assert(!nonConvexPoly5.isConvex)
  }

}

/**
  * Testing points being on a segment
  */
class PointSegmentTests extends FlatSpec with MustMatchers {

  val (s1, p1) = ((Point2D(-1, -1), Point2D(1, 1)), (0, 0))
  val (s2, p2) = ((Point2D(-1, -1), Point2D(1, 1)), (0, 0.001))
  val (s3, p3) = ((Point2D(-1, -1), Point2D(1, 1)), (2, 2))

  s"Point $p1" should s"be in segment $s1" in {
    assert(pointOnSegment(p1, s1))
  }

  s"Point $p2" should s"be not in segment $s2" in {
    assert(!pointOnSegment(p2, s2))
  }

  s"Point $p3" should s"be not in segment $s3" in {
    assert(!pointOnSegment(p3, s3))
  }

}


/**
  * Testing whether a point is in a polygon
  */
class PointInPolygonTests extends FlatSpec with MustMatchers {

  val p1 = (1, 1)
  val p2 = (1, 0.5)
  val p3 = (2, 3)
  val p4 = (0.5, 0.5)

  s"Point $p1" should s"be in triangle $trianglePoly" in {
    assert(trianglePoly.containsPoint(p1))
    assert(trianglePoly.containsPoint(origin2D))
    assert(trianglePoly.containsPoint((2, 2)))
  }

  s"Point $p2" should s"be in triangle $trianglePoly" in {
    assert(trianglePoly.containsPoint(p2))
  }

  s"Point $p3" should s"not be in triangle $trianglePoly" in {
    assert(!trianglePoly.containsPoint(p3))
  }

  s"Point $p2" should s"be in rectangle$convexPoly" in {
    assert(convexPoly.containsPoint(p2))
  }

  s"Point $p4" should s"be in rectangle$convexPoly3" in {
    assert(convexPoly3.containsPoint(p4))
    assert(convexPoly3.containsPoint((1, 0)))
  }

  s"Point $p4" should s"not be in rectangle$simpleNonConvexPoly" in {
    assert(!simpleNonConvexPoly.containsPoint(p4))
  }

}

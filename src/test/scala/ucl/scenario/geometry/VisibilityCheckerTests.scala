package ucl.scenario.geometry

import org.scalacheck.Prop._
import org.scalacheck.Properties
import org.scalatest.prop.Checkers
import org.scalatest.{FlatSpec, FunSuite, Matchers}
import ucl.scenario.geometry.PointUtils._
import ucl.scenario.geometry.examples.UsefulPolygons._
import ucl.scenario.geometry.generators.PolygonCombinatorUtils._
import ucl.scenario.geometry.generators.{CompositePolygon, PolygonPropertyUtils}
import ucl.scenario.geometry.triangles.{DualGraphUtils, Triangle}
import ucl.scenario.geometry.visibility.VisibilityChecker._
import ucl.scenario.geometry.visibility.{JoeSimpsonVisibility, VisibilityChecker}

/**
  * @author Ilya Sergey
  */

class TrianglePointSplittingTests extends FlatSpec with Matchers {

  def triangleEdgePointSplitTest(t: Triangle, p: Point2D, t1: Triangle, t2: Triangle): Unit = {
    s"$t" should s"be split by $p into $t1 and $t2" in {

      val ts = splitTriangleByEdgePoint(t, p)
      for (y <- ts) {
        assert(y =~= t1 && y =!= t2 || y =~= t2 && y =!= t1, s"Something is wrong here for $t and $p")
      }
    }
  }

  val p1 = (1, 0)
  val p2 = (2, 1)
  val p3 = (1, 1)

  triangleEdgePointSplitTest(triangle1, p1, new Triangle((2, 2), (1, 0), (2, 0)), new Triangle((0, 0), (1, 0), (2, 2)))
  triangleEdgePointSplitTest(triangle1, p2, new Triangle((2, 2), (0, 0), (2, 1)), new Triangle((0, 0), (2, 0), (2, 1)))
  triangleEdgePointSplitTest(triangle1, p3, new Triangle((1, 1), (2, 0), (2, 2)), new Triangle((2, 0), (1, 1), (0, 0)))

}

class TriangleSegmentSplittingTests extends FlatSpec with Matchers {

  def triangleSegmentTest(t: Triangle, s: Segment, num: Int): Unit = {
    s"$t" should s"be split by $s correctly" in {

      val ts = splitTriangleBySegment(t, s)
      assert(ts.size == num, s"Something is wrong here for $t and $s (1)")
      assert(ts.forall(t => t.containsPoint(t.center)), s"Something is wrong here for $t and $s (2)")

      //      for (y <- ts) {
      //        println(y)
      //      }
      //      println()
    }
  }

  val s1 = Segment((1, 0), (1, 1))
  val s2 = Segment((0, 0), (2, 1))
  val s3 = Segment((0, 0), (2, 3))
  val s4 = Segment((0, 0), (3, 3))
  val s5 = Segment((-1, 1), (-2, 2))
  val s6 = Segment((3, 3), (0.5, -0.5))

  triangleSegmentTest(triangle1, s1, 3)
  triangleSegmentTest(triangle1, s2, 2)
  triangleSegmentTest(triangle1, s3, 1)
  triangleSegmentTest(triangle1, s4, 1)
  triangleSegmentTest(triangle1, s5, 1)
  triangleSegmentTest(triangle1, s6, 3)

}

class TrianglePolygonSplittingTests extends FlatSpec with Matchers {

  def trianglePolygonTest(t: Triangle, vp: Polygon, num: Int): Unit = {
    s"$t" should s"be split by $vp correctly" in {
      val ts = splitTriangleByPolygon(t, vp)
      assert(ts.forall(t => t.containsPoint(t.center)), s"Something is wrong here for $t and $vp (1)")
      assert(ts.size == num, s"Something is wrong here for $t and $vp (2)")
      //      for (y <- ts) {
      //        println(y)
      //      }
      //      println()
    }
  }

  trianglePolygonTest(triangle1, triangle2, 5)

}

class VisibilityCheckerTests extends FlatSpec with Matchers {

  import VisibilityChecker.FailReason._

  def visibilityCheckerTest(p: Polygon, ps: Seq[Point2D]): Unit = {
    s"A polygon $p" should s"be all covered by $ps" in {
      val (b, reason, _) = checkVisibility(p, ps)
      assert(b, "It should be all visible")
    }
  }

  def visibilityCheckerFalseOutside(p: Polygon, ps: Seq[Point2D]): Unit = {
    s"A polygon $p" should s"be all covered by $ps" in {
      val (b, reason, _) = checkVisibility(p, ps)
      assert(!b, "It should fail")
      val pr = reason.get
      assert(pr._2 == Outside, "Checker should detect an outside point.")
    }
  }

  def visibilityCheckerFalseNonVisible(p: Polygon, ps: Seq[Point2D]): Unit = {
    s"A polygon $p" should s"be all covered by $ps" in {
      val (b, reason, _) = checkVisibility(p, ps)
      assert(!b, "It should fail")
      val pr = reason.get
      // println(pr._1)
      assert(pr._2 == NotVisible, "Checker should detect an outside point.")
    }
  }

  val s0 = Seq(Point2D(-1, -1))

  visibilityCheckerFalseOutside(kittyPolygon, s0)

  val s1 = Seq(Point2D(1, 1))
  val s2 = Seq(Point2D(0, 0))
  val s3 = Seq(Point2D(1, 0.5))
  val s4 = Seq(Point2D(0, 0), Point2D(2, 2))
  val s5 = Seq(Point2D(2, 1), Point2D(0, 3))
  val s6 = Seq(Point2D(3, 3), Point2D(0, 0))

  visibilityCheckerTest(trianglePoly, s1)
  visibilityCheckerTest(convexPoly, s1)
  visibilityCheckerTest(convexPoly, s2)
  visibilityCheckerTest(simpleNonConvexPoly, s4)
  visibilityCheckerTest(simpleNonConvexPoly, s3)
  visibilityCheckerTest(lShapedPolygon, s1)
  visibilityCheckerTest(weirdRectPolygon, s6)

  val s7 = Seq(Point2D(2, 1))
  val s8 = Seq(Point2D(3, 3), Point2D(2, 1))
  val s9 = Seq(Point2D(0, 2), Point2D(5, 2))
  val s10 = Seq(Point2D(5, 0), Point2D(0, 5), Point2D(-2, -2))

  visibilityCheckerFalseNonVisible(lShapedPolygon, s7)
  visibilityCheckerFalseNonVisible(weirdRectPolygon, s8)
  visibilityCheckerFalseNonVisible(kittyPolygon, s9)
  visibilityCheckerFalseNonVisible(shurikenPolygon, s10)

  visibilityCheckerFalseNonVisible(sand4, Seq((0, 1)))
  visibilityCheckerFalseNonVisible(sand3, Seq((3, 6)))
  visibilityCheckerFalseNonVisible(sand5, Seq((2.4, 4)))
  visibilityCheckerFalseNonVisible(lshaped3, Seq((0, 0), (1.7, 3.2)))
  visibilityCheckerFalseNonVisible(polarBug, Seq(Point2D(5, 2)))
  visibilityCheckerFalseNonVisible(piBug, Seq(Point2D(0, 0)))
  visibilityCheckerFalseNonVisible(keyBug, Seq(Point2D(3, 3)))
  visibilityCheckerFalseNonVisible(strangeKey, Seq(Point2D(7, 3)))

  visibilityCheckerTest(lshaped3, Seq((0, 0), (1.7, 3.2), (1.1, 1.1)))

}

class VisibilityPropertyTests extends FunSuite with Checkers {

  import PolygonPropertyUtils._
  import ucl.scenario.geometry.generators.RandomCrazyPolygonGenerator._

  test("All visibility polygons lie within the original polygon") {
    check { (p: CompositePolygon) =>
      collect(polygonCombinatorCollector(p)) {
//        println(s"Testing polygon $p")
        val guards = p.vertices
        val vpsr = guards.map(JoeSimpsonVisibility.visibilityPolygon(p, _))
        ("All visibility polygons are defined" |: vpsr.forall(_.isDefined)) &&
            ("Middle of every edge of a visibility polygon is within p" |: {
              val vps = vpsr.map(_.get)
              val cs = for (vp <- vps; e <- vp.edges) yield e.middle
              cs.forall(p.containsPoint)
            })
      }
    }
  }

  test("Fisk's algorithm is correct") {
    check { (p: CompositePolygon) =>
      collect(polygonCombinatorCollector(p)) {
        val guards = DualGraphUtils.chavatalVisibilitySet(p)
        val (b, _, _) = checkVisibility(p, guards)
        b
      }
    }
  }

  test("Fisk's algorithm result is within the boundary") {
    check { (p: CompositePolygon) =>
      collect(polygonCombinatorCollector(p)) {
        val guards = DualGraphUtils.chavatalVisibilitySet(p)
	guards.size <= p.vertices.size / 3
      }
    }
  }

}

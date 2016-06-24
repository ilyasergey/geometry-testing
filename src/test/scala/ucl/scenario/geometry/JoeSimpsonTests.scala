package ucl.scenario.geometry

import org.scalatest.{Matchers, FlatSpec}
import CommonUtils._
import PointUtils._
import ucl.scenario.geometry.examples.UsefulPolygons
import ucl.scenario.geometry.visibility.{VisibilityUtil, JoeSimpsonVisibility}
import JoeSimpsonVisibility._
import VisibilityUtil._
import UsefulPolygons._


/**
  * @author Ilya Sergey
  */

class VisibilityPreludeTests extends FlatSpec with Matchers {

  val p3 = (0.5, 0.5)
  val p1 = (0, 0)
  val p2 = (0, 1)
  val p4 = (0, 1)
  val p5 = (0, 2)


  def prepareForVisibility(p: Polygon, z: Point2D): Unit = {
    val (vs, angle, rotNum) = visibilityPrologue(z, p)

    val v0 = vs(0)
    assert(scala.math.abs(v0.phi) == 0, s"New initial node: $v0's polar angle is not 0")

    // Rotate by the inverse angle to get the initial polygon (modulo sequence shift)
    val vss: Seq[PointPolar] = (if (vs.zIsOrigin) Seq(origin2D.toPolar) else Seq()) ++
        (for (i <- 0 until vs.n) yield vs(i).point)

    val tmp1 = vss.map(_.rotateClockWise(-angle))
    // Rotate the sequence of vertices back to the initial one
    val tmpPol = rotateSeqNum(tmp1, -rotNum)
    // Convert back to Cartesisan
    val tmpCart = tmpPol.map(_.toCart)
    // Shift backwards, so point z would be at its initial location
    val finalPoly = Polygon(tmpCart.map(_ + Direction(z.x, z.y)))
    assert(p =~= finalPoly, s"Initial polygon $p is significantly different from resulting polygon $finalPoly")

  }

  def prepareTest(p: Polygon, z: Point2D): Unit = {
    s"A polygon $p" should s"be processed correctly wrt. to the point $z" in {
      prepareForVisibility(p, z)
    }
  }

  prepareTest(convexPoly3, p3)
  prepareTest(convexPoly3, p1)
  prepareTest(convexPoly3, p2)
  prepareTest(convexPoly3, p5)
  prepareTest(simpleNonConvexPoly, p4)

}

class AngularDisplacementTests extends FlatSpec with Matchers {

  def checkWindings(p: Polygon) = {
    val vts = p.vertices
    val polars = vts.map(_.toPolar)
    val vs = angularDisplacementSeq(polars)
    val alphas = vs.map(_.alpha)
    // Convert alphas back to squate coordinates
    val ps = vs.map(x => PointPolar(x.point.rad, x.alpha).toCart)
    assert(seqEpsEq(vts, ps), s"Alphas $alphas \nare computed wrong for the polygon $p.")
  }

  s"A simple centered convex polygon $square" should s"should be given  correct windings" in {
    checkWindings(square)
  }

  s"A simple centered convex polygon $convexPoly3" should s"should be given  correct windings" in {
    checkWindings(convexPoly3)
  }

  s"A simple centered non-convex polygon $simpleNonConvexPoly" should s"should be given  correct windings" in {
    checkWindings(simpleNonConvexPoly)
  }

}

class JoeSimpsonTests extends FlatSpec with Matchers {

  def checkVisibility(pol: Polygon, z: Point2D, exp: Boolean = true, checkSelf: Boolean = false): Unit = {
    s"Visibility polygon of $pol from internal point $z" should s"should be  the polygon itself" in {
      val res = visibilityPolygon(pol, z)
      assert(res.isDefined == exp, s"Visibility polygon is defined.")

      //      if (res.isDefined) {
      //        println(res.get)
      //      }

      if (pol.isConvex || checkSelf) {
        val rg = res.get
        assert(pol =~= rg, s"In this case the vsibility polygon should be the polygon itself:\n$pol\n$rg")
      }
    }
  }

  checkVisibility(trianglePoly, origin2D)
  checkVisibility(trianglePoly, (1, 0.5))
  checkVisibility(square, origin2D)
  checkVisibility(convexPoly3, origin2D)
  checkVisibility(convexPoly3, (1, 0))
  checkVisibility(simpleNonConvexPoly, (1, 1), exp = false)

  // Still all visible from the point, even though non-convex
  checkVisibility(simpleNonConvexPoly, (2, 0.5), checkSelf = true)
  checkVisibility(simpleNonConvexPoly, (1, 0.3), checkSelf = true)
  checkVisibility(simpleNonConvexPoly, (1, 0.0), checkSelf = true)
  checkVisibility(simpleNonConvexPoly, (1.9, 0.5), checkSelf = true)
  checkVisibility(simpleNonConvexPoly, (0, 0))
  checkVisibility(simpleNonConvexPoly, (2, 1.01))
  checkVisibility(simpleNonConvexPoly, (2, 2))
  checkVisibility(simpleNonConvexPoly, (1, 0.5), checkSelf = true)

  s"Two visibilities for L-shaped polygon $lShapedPolygon" should "be equivalent (1)" in {
    val res1 = visibilityPolygon(lShapedPolygon, (1.3, 1))
    val res2 = visibilityPolygon(lShapedPolygon, (1.8, 1))
    assert(res1.get =~= res2.get)
    // The polygon is cleaned up (aligned node is removed)
    assert(res1.get =~= Polygon(Seq((0, 0), (2, 0), (2, 1), (0, 1))))
  }

  s"Two visibilities for L-shaped polygon $lShapedPolygon" should "be equivalent (2)" in {
    // A non-trivial equality check
    val res1 = visibilityPolygon(lShapedPolygon, (1.5, 0.5))
    val res2 = visibilityPolygon(lShapedPolygon, (1.6, 0.4))
    // A simpler test with similar properties
    // val res3 = visibilityPolygon(nonConvexPoly4, (1.1 ,0.6))
    assert(res1.get =~= res2.get)
  }

  checkVisibility(lShapedPolygon, (1, 1), checkSelf = true)
  checkVisibility(simpleStarPolygon, (1, 1), checkSelf = true)

  s"Visibilities for polygons $lShapedPolygon and $weirdRectPolygon" should "be equal (1)" in {
    val res1 = visibilityPolygon(lShapedPolygon, (1, 1))
    val res2 = visibilityPolygon(weirdRectPolygon, (1, 1))
    val rg1 = res1.get
    val rg2 = res2.get
    assert(rg1 =~= rg2, s"\n$rg1\n$rg2")
  }

  s"Visibilities for polygons $lShapedPolygon and $weirdRectPolygon" should "be equal (2)" in {
    val res1 = visibilityPolygon(lShapedPolygon, (1.1, 0.5))
    val res2 = visibilityPolygon(weirdRectPolygon, (1.1, 0.5))
    val rg1 = res1.get
    val rg2 = res2.get
    assert(rg1 =~= rg2, s"\n$rg1\n$rg2")
  }

  //  all above are ok


}

class JoeSimpsonNontrivialTests extends FlatSpec with Matchers {

  /**
    * More interesting cases
    */
  def checkVisible(pol: Polygon, z: Point2D, p: Point2D): Unit = {
    s"Visibility polygon of $pol from internal $z" should s"contain $p" in {
      val res = visibilityPolygon(pol, z)
      assert(res.isDefined, s"Visibility polygon is defined.")
      val rg = res.get
      assert(rg.containsPoint(p), s"Visibility polygon $rg does not contain $p")
    }
  }

  def checkNotVisible(pol: Polygon, z: Point2D, p: Point2D): Unit = {
    s"Visibility polygon of $pol from internal $z" should s" not contain $p" in {
      val res = visibilityPolygon(pol, z)
      assert(res.isDefined, s"Visibility polygon is defined.")
      val rg = res.get
      assert(!rg.containsPoint(p), s"Visibility polygon $rg contains $p!")
    }
  }

  checkVisible(kittyPolygon, (0, 2), (1.5, 0.5))
  checkVisible(kittyPolygon, (0, 2), (2, 0))

  checkNotVisible(kittyPolygon, (0, 2), (1.5, 0.6))
  checkNotVisible(kittyPolygon, (0, 2), (2.1, 0))
  checkNotVisible(kittyPolygon, (0, 2), (2, 1))

  checkVisible(simpleStarPolygon, (0, 0), (3, 0))
  checkNotVisible(simpleStarPolygon, (3, 0), (0, 2))

  // [BUG 12-01-16]
  checkNotVisible(lShapedPolygon, (0.5, 2), (2, 0))

  s"A polygon $chvatal_comb" should s"be processed correctly" in {
    val p = Point2D(1.5, 1)
    val v = visibilityPolygon(chvatal_comb, p)
    assert(v.isDefined)
    val vp = v.get
    // println(vp)
    assert(vp.containsPoint((5, 1)), "Something wrong here")
  }



}
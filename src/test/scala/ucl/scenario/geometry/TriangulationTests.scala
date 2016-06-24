package ucl.scenario.geometry

import org.scalacheck.Prop._
import org.scalacheck.Properties
import org.scalatest.prop.{Checkers, GeneratorDrivenPropertyChecks}
import org.scalatest.{ShouldMatchers, FunSuite, Matchers, FlatSpec}
import ucl.scenario.geometry.triangles.Triangulation
import Triangulation._
import ucl.scenario.geometry.examples.UsefulPolygons
import UsefulPolygons._
import ucl.scenario.geometry.generators.{PolygonPropertyUtils, CompositePolygon}
import ucl.scenario.geometry.generators.PolygonCombinatorUtils._


/**
  * @author Ilya Sergey
  */

class TriangulationTests extends FlatSpec with Matchers {

  def triangulationTest(p: Polygon, perv: Boolean = false, n: Int = 0): Unit = {
    val m = if (perv) n else p.vertices.size - 2
    s"A polygon $p" should s"be triangulated into $m triangles" in {
      val ts = triangulate(p)
      val sz = ts.size
      assert(sz == m, s"The size is in fact $sz.")
      //      println(s"Initial polygon: $p\nTriangles:")
      for (t <- ts) {
        // Check centers and vertices of triangles
        // println(t)
        val c = t.center
        assert(p.containsPoint(c), s"$p doesn't contain $c.")
        for (v <- t.vertices) {
          assert(p.vertices.exists(_ =~= v), s"$p doesn't contain a vertex $v of $t")
        }
      }
      //      println
    }
  }

  triangulationTest(trianglePoly)
  triangulationTest(convexPoly)
  triangulationTest(simpleNonConvexPoly)
  triangulationTest(lShapedPolygon)
  triangulationTest(kittyPolygon)
  triangulationTest(simpleStarPolygon)
  triangulationTest(weirdRectPolygon)
  triangulationTest(tHorror)
  triangulationTest(triangBug)
}

class TriangulationPropertyTests extends FunSuite with Checkers {

  import PolygonPropertyUtils._
  import ucl.scenario.geometry.generators.RandomCrazyPolygonGenerator._

  test("Property: centers of all triangles are in polygon") {
    check((p: CompositePolygon) => {
      val trs = Triangulation.triangulate(p)
      trs.forall(t => p containsPoint t.center)
    })
  }
}

object TriangulationSpecification extends Properties("Triangulation") {

  import PolygonPropertyUtils._
  import ucl.scenario.geometry.generators.RandomRectilinearPolygonGenerator._

  val triangulationInside = forAll { (p: CompositePolygon) =>
    collect(polygonCombinatorCollector(p)) {
      //println(s"Testing polygon $p")
      val trs = Triangulation.triangulate(p)
      trs.forall(t => p.containsPoint(t.center))
    }
  }

  val triangulationCount = forAll { (p: CompositePolygon) =>
    collect(polygonCombinatorCollector(p)) {
      //println(s"Testing polygon $p")
      val trs = Triangulation.triangulate(p)
      trs.size == p.size - 2
    }
  }

  property("Center of each triangle lies within a polygon") = triangulationInside
  property("A number of (possibly degenerate) triangles is n − 2") = triangulationCount
}

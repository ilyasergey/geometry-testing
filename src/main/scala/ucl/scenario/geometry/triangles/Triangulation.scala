package ucl.scenario.geometry.triangles

import ucl.scenario.geometry.PointUtils._
import ucl.scenario.geometry.SegmentUtils._
import ucl.scenario.geometry.{Point2D, Polygon, Segment, Turn}
import ucl.scenario.geometry._

/**
  * Triangulations of simple polygons
  *
  * @author Ilya Sergey
  */

object Triangulation {

  /**
    * A naive quadratic triangulation algorithm
    */
  def triangulate(pol: Polygon): Set[Triangle] = {
    val vs = pol.getVertices
    if (vs.size <= 2) return Set.empty
    if (vs.size == 3) return Set(mkTriangle(vs))
    val (diag, i, j) = findDiagonal(pol)

    // Split into two new polygons
    val p1 = Polygon(vs(j) :: vs.slice(i, j))
    val p2 = Polygon(vs(i) ::
                     vs.slice(j, vs.size) ++ vs.slice(0, i))
    triangulate(p1) ++ triangulate(p2)
  }

  def mkTriangle(vs: Seq[Point2D]) =
    new Triangle(vs.head, vs(1), vs(2)).canonical

  def findDiagonal(pol: Polygon) = {
    val vs = pol.vertices
    val es = pol.edges
    val candidates =
      for {i <- vs.indices.toStream // necessary for laziness
           j <- i until vs.size
           cand = Segment(vs(i), vs(j))
           c1 = pol.containsPoint(cand.middle)
           c2 = !es.exists(e => e =~= cand || e.flip =~= cand)
           c3 = vs.forall(v => v =~= cand.a || v =~= cand.b ||
                          !cand.contains(v))
           // [bug] happens if to change to intersect
           c4 = es.forall(e => !intersectProper(cand, e))
           if c1 && c2 && c3 && c4
      } yield (cand, i, j)
    candidates.head
  }

  def findDiagonal1(pol: Polygon): Option[(Segment, Int, Int)] = {
    val vs = pol.vertices
    val es = pol.edges
    for (i <- vs.indices) {
      val vi = vs(i)
      for (j <- i until vs.size) {
        val vj = vs(j)
        val cand = Segment(vi, vj)
        // Candidate diagonal's middle lies in the polygon (1)
        // Candidate is not an edge (2)
        // Candidate doesn't contain any other vertices of the polygon (3)
        // No other edges intersect it properly (4)
        val cond1 = pol.containsPoint(cand.middle)
        // [BUG-354]
        // The part "|| e.flip =~= cand" is new and was in fact introduced after rigorous testing
        // Check the counterexample tHorror
        val cond2 = !es.exists(e => e =~= cand || e.flip =~= cand)
        val cond3 = vs.forall(v => v =~= cand.a || v =~= cand.b || !pointOnSegment(v, cand))
        // The following condition used to be more complex and, in fact, incorrect
        // [FIXED bug via rigorous testing]
        val cond4 = es.forall(e => !intersectProper(cand, e))
        if (cond1 && cond2 && cond3 && cond4) {
          //          assert(cand.a =!= cand.b)
          //          assert(i < j)
          return Some((cand, i, j))
        }
      }
    }
    None
  }

}


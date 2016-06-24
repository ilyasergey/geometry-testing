package ucl.scenario.geometry

import org.scalatest.{Matchers, FlatSpec}
import ucl.scenario.geometry.examples.UsefulPolygons
import ucl.scenario.geometry.visibility.{DumbGuards, VisibilityChecker}
import VisibilityChecker._
import UsefulPolygons._


/**
  * @author Ilya Sergey
  */

class DumbGuardsTests extends FlatSpec with Matchers {

  def dumbTest(p: Polygon): Unit = {
    s"Dumb 'all-but-last-two' guards placement algorithm for $p" should s"always succeed" in {
      val res = DumbGuards.dumbVisibility(p)
      assert(res.isDefined)
      val guards = res.get
      //      println(guards)
      val (b, reason, _) = checkVisibility(p, guards)
      assert(b, "but should be all visible")
    }
  }

  dumbTest(trianglePoly)
  dumbTest(lshaped3)
  dumbTest(tHorror)
  dumbTest(sand5)
  dumbTest(weirdRectPolygon)
  dumbTest(simpleStarPolygon)
  dumbTest(shurikenPolygon)

}

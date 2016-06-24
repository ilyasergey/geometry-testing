package ucl.scenario.geometry

import SegmentUtils._

/**
  * @author Ilya Sergey
  */

object RenderUtils {

  /**
    * Returns bottom-left and top-right corners of the polygon box
    */
  def getPolygonBox(p: Polygon): (Point2D, Point2D) = {
    val vs = p.vertices
    val (p1, p2) = vs.foldLeft((0.0, 0.0), (0.0, 0.0)) {
      (z, v) =>
        val x1 = math.min(z._1._1, v.x)
        val y1 = math.min(z._1._2, v.y)
        val x2 = math.max(z._2._1, v.x)
        val y2 = math.max(z._2._2, v.y)
        ((x1, y1), (x2, y2))
    }
    (Point2D(p1._1, p1._2), Point2D(p2._1, p2._2))
  }

  def getGraphicParameters(p: Polygon) = {
    val (botl, topr) = getPolygonBox(p)
    val mid = (botl, topr).middle
    val width = topr.x - botl.x
    val height = topr.y - botl.y
    // Scale coefficient
    val k = 600 / math.max(width, height)

    // Overall graphic area is within 800 * 800 pixels
    val xShift = k * (mid.x - botl.x)
    val yShift = k * (mid.y - botl.y)
    val gWidth = k * width + 200
    val gHeigth = k * height + 200

    (gWidth.toInt, gHeigth.toInt, k.toInt, xShift.toInt, 200 + yShift.toInt)
  }

}

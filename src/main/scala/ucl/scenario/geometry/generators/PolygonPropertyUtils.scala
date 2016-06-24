package ucl.scenario.geometry.generators

import org.scalacheck.{Arbitrary, Gen, Shrink}
import ucl.scenario.geometry.CommonUtils._
import ucl.scenario.geometry.examples.UsefulPolygons._
import ucl.scenario.geometry.generators.PolygonGenerators._
import ucl.scenario.geometry._

/**
  * @author Ilya Sergey
  */

object PolygonPropertyUtils {


  /**
    * Construct an aligned rectangular polygon with the bottom-right
    * angle in the section
    */
  val generateNormalizedRectangle: LazyPolygon =
    (longEdgeSize: Int) =>
      Polygon(Seq(Point2D(0, 0), Point2D(0, longEdgeSize),
        Point2D(-1, longEdgeSize), Point2D(-1, 0)))

  def polyFreqs[T](fs: Seq[Int], ps: Seq[T]): Gen[T] =
    Gen.frequency(fs.zip(ps.map(p => Gen.const(p))): _*)

  val polygonCombinatorCollector = (pc: CompositePolygon) => {
    val n = pc.size
    if (n <= 60) s"Small-size polygon (<= 60 vertices)"
    else if (n > 60 && n <= 150) s"Medium-size polygon (61..150 vertices)"
    else if (n > 150 && n <= 300) s"Large-size polygon (151..300 vertices)"
    else s"Very large-size polygon (>300 vertices)"
  }

  def generateConvexPolygon(n: Int): Polygon = {
    assert(n > 2)
    val frac = PI2 / n
    val vs: Seq[Point2D] = for (i <- 0 until n; phi = i * frac) yield PointPolar(1, phi).toCart
    Polygon(vs)
  }

  implicit val shrinkPolygonCombinator: Shrink[CompositePolygon] =
    Shrink { (p: CompositePolygon) =>
      PolygonCombinatorUtils.makeTraversals(p)
    }

  def polygonGenerator(basePolygons: Seq[Polygon], freqsBase: Seq[Int],
                       polygonsToAttach: Seq[LazyPolygon], freqsAtt: Seq[Int],
                       eSizeInt: (Int, Int),
                       generations: Int,
                       posStrategy: Double => Option[(Int, Int)]): Gen[CompositePolygon] = for {
  // choose base polygon
    base <- polyFreqs(freqsBase, basePolygons)
    // edge size
    eSize = Gen.choose(eSizeInt._1, eSizeInt._2)
    // number of extensions (generations)
    gSize <- Gen.choose(1, generations)
    // generate polygon
    attachments = polyFreqs(freqsAtt, polygonsToAttach)
  } yield generatePolygon(base, attachments, eSize, gSize, posStrategy)

}
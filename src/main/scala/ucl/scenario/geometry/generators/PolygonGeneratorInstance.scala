package ucl.scenario.geometry.generators

import org.scalacheck.Arbitrary
import ucl.scenario.geometry.Polygon
import ucl.scenario.geometry.examples.UsefulPolygons._
import ucl.scenario.geometry.generators.PolygonGenerators._
import ucl.scenario.geometry.generators.PolygonPropertyUtils._
import ucl.scenario.geometry.CommonUtils._


/**
  * @author Ilya Sergey
  */

abstract class PolygonGeneratorInstance {

  val basePolygons: Seq[Polygon]
  val freqsBase: Seq[Int]
  val polygonsToAttach: Seq[LazyPolygon]
  val freqsAtt: Seq[Int]
  val attachedYSize: (Int, Int)
  val generations: Int
  // Positioning might fail if the edge is not suitable
  val positionStrategy: Double => Option[(Int, Int)]

  implicit lazy val arbPolygonCombinator: Arbitrary[CompositePolygon] =
    Arbitrary(polygonGenerator(basePolygons, freqsBase, polygonsToAttach, freqsAtt,
      attachedYSize, generations, positionStrategy))

}

// TODO: emplain these generators and higher-order strategies
object RandomCrazyPolygonGenerator extends PolygonGeneratorInstance {

  val basePolygons = Seq(simple3Rectangle, flat3Polygon, weirdRectPolygon,
    chvatal_comb, simpleStarPolygon, lshaped3, sand5)

  val freqsBase = Seq(20, 20, 3, 2, 2, 1, 1)


  val polygonsToAttach = Seq(
    generateNormalizedRectangle,
    prep(chvatal_comb),
    prep(kittyPolygon),
    prep(triangle3),
    prep(triangle2)
  )

  val freqsAtt = Seq(10, 2, 5, 2, 1)
  val attachedYSize = (3, 8)
  val generations = 20

/*
  val positionStrategy = (l: Double) => {
    val startOffset = randomIntBetween(0, l.toInt - 1)
    val endOffset = randomIntBetween(startOffset + 1, l.toInt - 1)
    Some((startOffset, endOffset))
  }
*/
  // better positions strategy
  val positionStrategy = (l: Double) => {
    if (l < 3) None
    else {
      val startOffset = randomIntBetween(1, l.toInt - 2)
      val endOffset = randomIntBetween(startOffset + 1, l.toInt - 1)
      Some((startOffset, endOffset))
    }
  }


}


object RandomRectilinearPolygonGenerator extends PolygonGeneratorInstance {

  val basePolygons = Seq(simple3Rectangle)
  val freqsBase = Seq(1)
  val polygonsToAttach = Seq(generateNormalizedRectangle)
  val freqsAtt = Seq(1)
  val attachedYSize = (2, 8)
  val generations = 50

  val positionStrategy = (l: Double) => {
    if (l < 3) None
    else {
      val startOffset = randomIntBetween(1, l.toInt - 2)
      val endOffset = randomIntBetween(startOffset + 1, l.toInt - 1)
      Some((startOffset, endOffset))
    }
  }

}


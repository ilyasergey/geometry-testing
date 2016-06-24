package ucl.scenario.util

import ucl.scenario.geometry.{Point2D, Polygon}
import ucl.scenario.geometry.runners.BasicGeometryParsers

import scala.util.parsing.combinator.JavaTokenParsers
import scala.xml.Elem

/**
  * @author Ilya Sergey
  */

object InputUtils {

  val TOO_FEW_LINES_MSG = <p>Your input file should contain at least two lines: with the group name and password.</p>

  val NO_SUCH_TEAM_MSG = (tname: String) => <p>There is no team with the name
    <b>
      {tname}
    </b>
    .</p>

  val NO_PW_FOR_TEAM_MSG = (tname: String) => <p>No password set up for the team
    <b>
      {tname}
    </b>
    .</p>

  val WRONG_PASS_MSG = (tname: String) => <p>Wrong password for the team
    <b>
      {tname}
    </b>
  </p>

  val OK_MSG = <p>OK</p>

  def splitStringIntoLines(s: String): Seq[String] = s.split("\\r?\\n+").toSeq.filter(!_.trim.isEmpty)

  /**
    *
    * @param sDirPath the path to the scenario instance folder
    * @param lines input file lines
    * @return
    */
  def preValidateInputText(sDirPath: String, lines: Seq[String]): (Boolean, Elem, Option[(String, Seq[String])]) = {
    import ProjectUtils._
    if (lines.length < 2) {
      return (false, TOO_FEW_LINES_MSG, None)
    }
    val tname = lines.head
    val tpass = lines(1)

    if (!teamFolderExists(sDirPath, tname)) {
      return (false, NO_SUCH_TEAM_MSG(tname), None)
    }

    val pwr = getTeamPassword(sDirPath, tname)
    if (pwr.isEmpty) {
      return (false, NO_PW_FOR_TEAM_MSG(tname), None)
    }

    val pw = pwr.get
    if (!pw.equals(tpass)) {
      return (false, WRONG_PASS_MSG(tname), None)
    }

    (true, OK_MSG, Some((tname, lines.drop(2))))
  }

}

/**
  * Parsing polygons to guard
  */

/*
For the Guards problem (I), the format for input polygon lines is as follows:

Num : Vertices

For instance, the following is polygon number 17 with 6 vertices:

17: (0, 0), (5, 0), (5, 2), (4, 1), (1, 1), (0, 2)

 */

object GuardInputParser extends BasicGeometryParsers {
  def line: Parser[(Int, Polygon)] = (wholeNumber <~ ":") ~ points ^^ {
    case num ~ vs => (num.toInt, Polygon(vs))
  }

  def apply(s: String) = parseAll(line, s)
}

/*
For the Guards problem (I), the format for solution file lines is as follows:

Num : Guards

For instance, the following is polygon number 17 with 3 suggested guards

17: (0, 0), (1, 1), (5, 2)
 */

object GuardSolutionParser extends BasicGeometryParsers {
  def line: Parser[(Int, Seq[Point2D])] = (wholeNumber <~ ":") ~ points ^^ {
    case num ~ guards => (num.toInt, guards)
  }

  def apply(s: String) = parseAll(line, s)
}

/*
Parsing serialized results in the format

 pID : guardsSize / numOfVertices

 For instance,

3: 5 / 16
 */
object GuardResultParser extends BasicGeometryParsers {
  def line: Parser[(Int, Int)] = (wholeNumber <~ ":") ~ wholeNumber <~ ("/" ~ wholeNumber) ^^ {
    case pNum ~ gNum => (pNum.toInt, gNum.toInt)
  }

  def apply(s: String) = parseAll(line, s)
}

/**
  * Parsing polygons/guards to check
  */

/*
For the Check problem (II), the format for input polygon lines is as follows:

Num : Vertices ; Guards

For instance, the following is polygon number 17 with 6 vertices and 1 guard

17: (0, 0), (5, 0), (5, 2), (4, 1), (1, 1), (0, 2) ; (4, 1)

 */
object CheckInputParser extends BasicGeometryParsers {
  def line: Parser[(Int, (Polygon, Seq[Point2D]))] = (wholeNumber <~ ":") ~ points ~ (";" ~> points) ^^ {
    case num ~ vs ~ gs => (num.toInt, (Polygon(vs), gs))
  }

  def apply(s: String) = parseAll(line, s)
}


/*
For the Check problem (II), the format for solution file lines is as follows:

Num : Point

For instance, the following is a proposed thief position for polygon 17

17: (0.1, 0.1)


 */

object CheckSolutionParser extends BasicGeometryParsers {
  def line: Parser[(Int, Point2D)] = (wholeNumber <~ ":") ~ point ^^ {
    case num ~ pt => (num.toInt, pt)
  }

  def apply(s: String) = parseAll(line, s)
}

// The result parser is the same as for the solution parser
object CheckResultParser extends BasicGeometryParsers {
  def apply(s: String) = CheckSolutionParser.apply(s)
}



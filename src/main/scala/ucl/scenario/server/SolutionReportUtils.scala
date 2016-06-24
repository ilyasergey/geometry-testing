package ucl.scenario.server

import java.util.Date

import ucl.scenario.util.ProjectUtils
import ucl.scenario.util.ProjectUtils.SubmissionType._

/**
  * @author Ilya Sergey
  */

object SolutionReportUtils {

  def reportDuplicatesError(): String =
    s"""
       |Unfortunately, your input file contains duplicated entries.
       |Please, check it carefully before submitting.
      """.stripMargin


  def reportParseErrors(parseErrors: Seq[String]): String = {
    s"""
       |Unfortunately, your input file contained the following formatting errors:
       |
      |${parseErrors.filter(_.nonEmpty).mkString("\n")}
       |
      |Please, check your solution file.
    """.stripMargin
  }

  def reportOutOfRangeErrors(oor: Seq[Int]): String =
    s"""
       |Unfortunately, your input file contained the following out-of range entries (there are no such polygon IDs in the task input):
       |
      |${oor.mkString(", ")}
       |
      |Please, check your solution file.
       |""".stripMargin


  def reportBadSetupException(e: String) =
    s"""
       |Unfortunately, something went wrong with the initial system setup, and the following exception was thrown:
       |
      | $e
       |
      |Please, report to the organizers.
    """.stripMargin


  def wrapMessage(d: Date, st: SubmissionType, msg: String, tname: String): String = {
    s"""
       |Dear members of the team "$tname".
       |
       |You have submitted a solution file for the ${toDescription(st)} at ${ProjectUtils.dateFormat.format(d)}.
       |
       |$msg
       |
       |Kind regards,
       |Scenario Week Team
    """.stripMargin
  }

}

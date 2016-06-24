package ucl.scenario.util

import java.util.{Calendar, Date}

import akka.actor.Actor
import ucl.scenario.util.ProjectUtils.ProjectConfig

/**
  * @author Ilya Sergey
  */

case class LogErrMessage(e: Throwable)

class LogActor(val config: ProjectConfig)
    extends Actor {

  def mkBody(e: Throwable): String = {

    val st = e.getStackTrace.mkString("\n")

    s"""Dear Scenario Week administrator.
        |
        |Something went wrong during the execution of the server. Please, find the details below:
        |
        |${e.getMessage}
        |
        |$st
        |
        |Kind regards,
        |Scenario Server
        |
        |P.S. Yes, I'm just a soulless machine, and I don't care about your problems, but you got to fix that ASAP.
      """.stripMargin
  }

  override def receive = {
    case LogErrMessage(e) =>
      val mr = config.reportMail
      if (mr.isEmpty) {
        System.err.println(e.getMessage)
        e.printStackTrace()
      } else {
        val time = ProjectUtils.dateFormat.format(Calendar.getInstance().getTime)
        val to = mr.get
        val body = mkBody(e)
        try {
          sendResult(to, time, body)
        } catch {
          // Oh, give me a break...
          case e: Throwable =>
            System.err.println(e.getMessage)
            e.printStackTrace()
        }
      }
    case _ => // do nothing
  }

  def sendResult(addr: String, time: String, body: String) {
    import EmailUtils._
    val subj = s"Problem with Scenario Server at $time"
    new Mail(to = Seq(addr),
      subject = subj,
      message = body)
  }

}

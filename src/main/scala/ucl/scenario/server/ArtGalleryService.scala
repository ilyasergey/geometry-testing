package ucl.scenario.server

import java.util.Calendar

import akka.actor.{Actor, ActorRef}
import spray.http.MediaTypes._
import spray.http.StatusCodes._
import spray.http._
import spray.routing._
import ucl.scenario.util.InputUtils
import ucl.scenario.util.InputUtils.splitStringIntoLines
import ucl.scenario.util.ProjectUtils._

// we don't implement our route structure directly in the service actor because
// we want to be able to test it independently, without having to spin up an actor
class ArtGalleryActor(conf: ProjectConfig, workers: Map[String, ActorRef]) extends Actor with ArtGalleryService {

  val config = conf
  val teamWorkers = workers

  // the HttpService trait defines only one abstract member, which
  // connects the services environment to the enclosing actor or test
  def actorRefFactory = context

  // this actor only runs our route, but you could add
  // other things here, like request stream processing
  // or timeout handling
  def receive = runRoute(myRoute)
}

// this trait defines our service behavior independently from the service actor
trait ArtGalleryService extends HttpService {

  // Main scenario directory
  val config : ProjectConfig
  // Per-team actors
  val teamWorkers: Map[String, ActorRef]

  import ArtGalleryHtmlUtils._

  // Accessing main path
  private val emptyPathRoute = path("") {
    (get | post | put | delete) {
      redirect(s"/$homePath", MovedPermanently)
    }
  }

  private val homePathRoute = path(homePath) {
    (get | post | put | delete) {
      respondWithMediaType(`text/html`)(complete(homePage))
    }
  }

  private val scoreGuardsPathRoute = path(scoreboardGuardsPath) {
    (get | post | put | delete) (respondWithMediaType(`text/html`)(complete(scoreboardGuardsPage(config.root))))
  }

  private val scoreCheckPathRoute = path(scoreboardCheckPath) {
    (get | post | put | delete) (respondWithMediaType(`text/html`)(complete(scoreboardCheckPage(config.root))))
  }


  import SubmissionType._

  private val uploadCheckRoute =
    path(uploadCheckPath) {
      // respond to get with the default form
      get(respondWithMediaType(`text/html`)(complete(uploadCheckPage(config)))) ~ post {
        // Process submitted file
        processSubmittedFile(CheckSubmission)
      }
    }

  private val uploadGuardRoute =
    path(uploadGuardPath) {
      // respond to get with the default form
      get(respondWithMediaType(`text/html`)(complete(uploadGuardsPage(config)))) ~ post {
        // Process submitted file
        processSubmittedFile(GuardSubmission)
      }
    }

  lazy val processSubmittedFile = (st: SubmissionType) => {
    formFields("file" ?) {
      fileOpt => {
        val now = Calendar.getInstance().getTime
        val cDate = config.stopDate
        if (cDate.isDefined && cDate.get.before(now)) {
          respondWithMediaType(`text/html`)(complete(deadlinePassedText(toScorePath(st))))
        } else {
          if (fileOpt.isDefined) {
            val lines = splitStringIntoLines(fileOpt.get)
            val (b, msg, cont) = InputUtils.preValidateInputText(config.root, lines)
            if (b) {
              // start processing
              val (tname, solution) = cont.get
              if (teamWorkers.contains(tname)) {
                teamWorkers(tname) ! toSolution(st, lines.drop(2))
                respondWithMediaType(`text/html`)(complete(fileSubmittedText(st)))
              } else {
                respondWithMediaType(`text/html`)(complete(noWorkingActorText(tname, toWebPath(st))))
              }
            } else {
              respondWithMediaType(`text/html`)(complete(fileProblemText(msg, toWebPath(st))))
            }
          } else {
            respondWithMediaType(`text/html`)(complete(fileNotSubmittedText(toWebPath(st))))
          }
        }
      }
    }
  }

  val myRoute = emptyPathRoute ~ homePathRoute ~ uploadCheckRoute ~ uploadGuardRoute ~
      scoreGuardsPathRoute ~ scoreCheckPathRoute

}


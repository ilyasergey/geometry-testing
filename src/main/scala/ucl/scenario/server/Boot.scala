package ucl.scenario.server

import java.io.File

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.io.IO
import spray.can.Http
import akka.pattern.ask
import akka.util.Timeout
import ucl.scenario.util.ProjectUtils.ProjectConfig
import ucl.scenario.util.{LogActor, BadSetupException, ProjectUtils}
import scala.concurrent.duration._

object Boot extends App {

  private val DEFAULT_SCENARIO_PATH = Seq(System.getProperty("user.dir"), "instances", "test").mkString(File.separator)

  val scenarioPath = {
    if (args.length == 0) DEFAULT_SCENARIO_PATH
    else {
      val path = args(0)
      val d = new File(path)
      if (!d.exists()) {
        throw new Exception(s"Scenario project in the path $path is not found.")
      }
      if (!d.isDirectory) {
        throw new Exception(s"Path $path doens't point to a directory.")
      }

      // Check or create missing directories
      ProjectUtils.checkOrCreateSolutionFolders(path)
      val (b, s) = ProjectUtils.checkProjectStructure(d)
      if (!b) {
        System.err.println(s)
        System.err.println()
        throw new BadSetupException(s)
      } else {
        d.getAbsolutePath
      }
    }
  }

  val myConfig : ProjectConfig = ProjectUtils.readProjectConfigFile(scenarioPath)

  // we need an ActorSystem to host our application in
  implicit val system = ActorSystem("art-gallery-webapp")

  lazy val logActor = system.actorOf(Props(new LogActor(myConfig)), name = s"log-actor")

  lazy val teamWorkers: Map[String, ActorRef] = {
    val workers = (for (tname <- ProjectUtils.getTeams(scenarioPath)) yield {
      val worker = system.actorOf(Props(new TeamWorker(scenarioPath, tname, logActor)), name = s"$tname-worker")
      (tname, worker)
    }).toMap
    workers
  }

  // create and start our service actor
  lazy val service = system.actorOf(Props(new ArtGalleryActor(myConfig, teamWorkers)), name = "art-gallery-service")


  implicit val timeout = Timeout(5.seconds)

  // start a new HTTP server with our service actor as the handler

  IO(Http) ? Http.Bind(service, interface = myConfig.host, port = myConfig.port)


}

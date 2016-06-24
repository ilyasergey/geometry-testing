package ucl.scenario.server

import java.io.File
import java.nio.file.{FileSystems, Files}
import java.util.Calendar

import ucl.scenario.util.ProjectUtils.{ProjectConfig, SubmissionType}
import ucl.scenario.util.ProjectUtils.SubmissionType.SubmissionType
import ucl.scenario.util.{CheckStatisticsUtils, GuardsStatisticsUtils, ProjectUtils}

import scala.xml.Elem

/**
  * Various HTML goodies for prettier page rendering
  *
  * @author Ilya Sergey
  */

object ArtGalleryHtmlUtils {

  val fileCheckerPath = "resources" + File.separator + "file_checker.js"

  def embedToPage(title: String, header: Elem, body: Elem, footer: Elem, fancy: Boolean = true): Elem =
    <html>
      <head>
        <title>
          {title}
        </title>
        {if (fancy) <link rel='stylesheet' type='text/css' href='http://ilyasergey.net/style.css'/>}
        <link href='http://fonts.googleapis.com/css?family=Judson|Raleway:500' rel='stylesheet' type='text/css'/>
      </head>
      <body>
        {header}<br/>{body}<br/>{footer}
      </body>
    </html>

  val homeHeader = <p>
    <center>
      <h1>Art Gallery Problem</h1>
      <h2>Scenario Week, 22-26 February 2016</h2>
    </center>
  </p>

  val homeBody = <p>
    <h3>Part 1: Computing Guards Set</h3>
    <ul>
      <li>
        <a href="/uploadGuards">Submit Solution File</a>
      </li>
      <li>
        <a href="/scoreGuards">Check Scoreboard</a>
      </li>
    </ul>
    <h3>Part 2: Checking Guards Set</h3>
    <ul>
      <li>
        <a href="/uploadCheck">Submit Solution File</a>
      </li>
      <li>
        <a href="/scoreCheck">Check Scoreboard</a>
      </li>
    </ul>
  </p>

  val homeFooter = <p>
    <hr/>
    <center>Department of Computer Science, University College London, February 2016</center>
  </p>

  val homePage = embedToPage("Art Gallery Problem", homeHeader, homeBody, homeFooter)


  val fileScriptString = {
    val path = System.getProperty("user.dir") + File.separator + fileCheckerPath
    val file = new File(path)
    assert(file.exists())
    val lines = Files.readAllLines(FileSystems.getDefault.getPath(file.getPath))
    val res = lines.toArray.toSeq.mkString(System.lineSeparator())
    res
  }

  val fileSizeCheckScript = <script type="text/javascript">
    {fileScriptString}
  </script>

  val uploadGuardHeader =
    <center>
      <h2>Uploading a solution for the Part 1</h2>
      <h3>Computing Guards Set</h3>
    </center>

  val uploadCheckHeader =
    <center>
      <h2>Uploading a solution for the Part 2</h2>
      <h3>Checking Guards Set</h3>
    </center>

  val scoreboardGuardHeader = (u: Unit) =>
    <center>
      <h2>Scoreboard for the Part 1</h2>
      <h3>Computing Guards Set</h3>
      <h4>{getTime()}</h4>
    </center>

  private def getTime() = ProjectUtils.dateFormat.format(Calendar.getInstance().getTime)

  val scoreboardCheckHeader = (u: Unit) =>
    <center>
      <h2>Scoreboard for the Part 2</h2>
      <h3>Checking Guards Set</h3>
      <h4>{getTime()}</h4>
    </center>

  val uploadBody = (uAction: String, config: ProjectConfig) => <center>
    { val now = Calendar.getInstance().getTime
      val cDate = config.stopDate
      if (cDate.isDefined && cDate.get.before(now)) {
        <p>Sorry, but the deadline for submissions has passed!</p>
      } else {
        <form action={uAction} method="post" enctype="multipart/form-data">
          {fileSizeCheckScript}<br/>
          <center>
            <input type="file" name="file" id="file" onchange="checkSize();"/>
          </center>
          <br/>
          <br/>
          <input type="submit" name="btn" id="btn" value="Upload Solution File" style="display: none"/>
        </form>
      }
    }
  </center>

  val backToMainFooter = <p>
    <center>
      <a href="/">Back to the main page</a>
    </center>
  </p>

  val fileSubmittedText = (st: SubmissionType) => embedToPage("Solution submitted",
    <p></p>,
    <center>Your solution has been submitted and will be processed shortly.</center>,
    <center>
      <br/> <a href={SubmissionType.toScorePath(st)}>Check the scoreboard</a>
      <br/>
      <br/> <a href="/">Back to the main page</a>
    </center>)

  val fileProblemText = (msg: Elem, backPath: String) => embedToPage("A problem with the submitted file.",
    <center>
      <h2>A problem with the submitted file</h2>
    </center>,
    <center>
      {msg}
    </center>,
    <center>
      <br/> <a href={backPath}>Back to the submission page</a>
    </center>)

  val fileNotSubmittedText = (backPath: String) => embedToPage("For some reasons, your file wasn't submitted. :(",
    <center>
      <h2>For some reasons, your file wasn't submitted. :(</h2>
    </center>,
    <center>Please, try to do it again.</center>,
    <center>
      <br/> <a href={backPath}>Back to the submission page</a>
    </center>)

  val deadlinePassedText = (backPath: String) => embedToPage("Deadline passed",
    <center>
      <h2>Deadline passed. :(</h2>
    </center>,
    <center>Sorry, but the deadline for submissions has passed.</center>,
    <center>
      <br/> <a href={backPath}>Check Scoreboard</a>
    </center>)

  val noWorkingActorText = (tname: String, backPath: String) => embedToPage(s"No actor for the team $tname",
    <center>
      <h2>No actor for the team
        {tname}
        .</h2>
    </center>,
    <center>Please, report to the organizers.</center>,
    <center>
      <br/> <a href={backPath}>Back to the submission page</a>
    </center>)

  val uploadGuardsPage = (config: ProjectConfig) =>
    embedToPage("Guards Solution Uploader", uploadGuardHeader, uploadBody("uploadGuards", config), backToMainFooter)

  val uploadCheckPage = (config: ProjectConfig) =>
    embedToPage("Check Solution Uploader", uploadCheckHeader, uploadBody("uploadCheck", config), backToMainFooter)

  val scoreboardGuardsPage = (sRoot: String) =>
    embedToPage("Guards Scoreboard", scoreboardGuardHeader(()), computeGuardsResults(sRoot), backToMainFooter, fancy = false)

  val scoreboardCheckPage = (sRoot: String) =>
    embedToPage("Check Scoreboard", scoreboardCheckHeader(()), computeCheckResults(sRoot), backToMainFooter, fancy = false)

  def computeGuardsResults(sRoot: String): Elem = {
    if (sRoot == null) <p></p>
    else <center>
      {GuardsStatisticsUtils.renderGuardsStatistics(sRoot)}
    </center>
  }

  def computeCheckResults(sRoot: String): Elem = {
    if (sRoot == null) <p></p>
    else <center>
      {CheckStatisticsUtils.renderCheckStatistics(sRoot)}
    </center>
  }

}

package ucl.scenario.util

import java.io._
import java.math.BigInteger
import java.nio.file.Files
import java.security.SecureRandom
import java.text.ParseException
import java.util.Date

import org.apache.commons.io
import ucl.scenario.geometry.{Point2D, Polygon}
import ucl.scenario.server.{CheckSolution, GuardsSolution, ScenarioMessage}
import ucl.scenario.util.EmailUtils.Mail

import scala.io.Source
import scala.util.Try

/**
  * Simple way to generate alphanumeric passwords
  *
  * @author Ilya Sergey
  */

object ProjectUtils {

  val homePath = "home"
  val uploadGuardPath = "uploadGuards"
  val uploadCheckPath = "uploadCheck"
  val scoreboardGuardsPath = "scoreGuards"
  val scoreboardCheckPath = "scoreCheck"

  val testUserName = "bender"
  val testPassword = "bitemyshinymetalass"

  val dateFormat = new java.text.SimpleDateFormat("HH:mm:ss, dd MMM yyyy")

  val tasksDir = "tasks"
  val teamsDir = "teams"
  val guardsDir = "guards"
  val checkDir = "check"
  val passwordFile = "password"
  val emailsFile = "emails"
  val feedbackFile = "feedback.txt"
  val configFile = "config"

  val guardsTaskFile = "guards.pol"
  val checkTaskFile = "check.pol"

  val resultsPrefix = "results."
  val dumpPrefix = "dump."
  val msgPrefix = "msg."
  val tmpSuffix = "tmp"

  val DEFAULT_HOST: String = "127.0.0.1"
  val DEFAULT_PORT: Int = 8083
  val DEFAULT_REPORT_MAIL: Option[String] = None
  val DEFAULT_STOP_DATE: Option[Date] = None

  val commentToken = "#"
  val hostKey = "host"
  val portKey = "port"
  val reportKey = "reportMail"
  val stopDate = "stopDate"

  private val DEFAULT_GUARDS_FILE = Seq(System.getProperty("user.dir"), "resources", "tasks", "guards", guardsTaskFile).mkString(File.separator)

  private val DEFAULT_CHECK_FILE = Seq(System.getProperty("user.dir"), "resources", "tasks", "check", checkTaskFile).mkString(File.separator)

  private val DEFAULT_CONFIG_FILE = Seq(System.getProperty("user.dir"), "resources", configFile).mkString(File.separator)

  case class ProjectConfig(host: String = DEFAULT_HOST,
                           port: Int = DEFAULT_PORT,
                           reportMail: Option[String] = DEFAULT_REPORT_MAIL,
                           stopDate: Option[Date] = DEFAULT_STOP_DATE,
                           root: String)

  def readProjectConfigFile(sRoot: String): ProjectConfig = {
    val cFile = new File(List(sRoot, configFile).mkString(File.separator))
    if (!cFile.exists()) return new ProjectConfig(root = sRoot)

    val map: Map[String, String] = try {
      val lines = Source.fromFile(cFile).getLines
      val kmap = lines.filter(!_.trim().startsWith(commentToken)).
          map(l => {
            val chunks = l.split(":", 2).toSeq.map(_.trim).filter(_.nonEmpty)
            assert(chunks.size == 2)
            (chunks.head, chunks.tail.head)
          }).toMap
      kmap
    } catch {
      case e: Throwable =>
        e.printStackTrace()
        Map.empty
    }

    try {
      val date = if (map.isDefinedAt(stopDate)) {
        try {
          Some(ProjectUtils.dateFormat.parse(map(stopDate)))
        } catch {
          case pe: ParseException => None
        }
      } else None

      ProjectConfig(
        host = map.getOrElse(hostKey, DEFAULT_HOST),
        port = Integer.parseInt(map.getOrElse(portKey, DEFAULT_PORT.toString)),
        reportMail = map.get(reportKey),
        stopDate = date,
        root = sRoot
      )
    } catch {
      case e: Throwable => e.printStackTrace()
        new ProjectConfig(root = sRoot)
    }
  }

  private val random = new SecureRandom()


  object SubmissionType extends Enumeration {
    type SubmissionType = Value
    val CheckSubmission, GuardSubmission = Value

    def toWebPath(s: SubmissionType): String = s match {
      case GuardSubmission => "/" + uploadGuardPath
      case CheckSubmission => "/" + uploadCheckPath
    }

    def toSolution(s: SubmissionType, content: Seq[String]): ScenarioMessage = s match {
      case GuardSubmission => GuardsSolution(content)
      case CheckSubmission => CheckSolution(content)
    }

    def toDescription(s: SubmissionType): String = s match {
      case GuardSubmission => "Problem 1: Computing Guards"
      case CheckSubmission => "Problem 2: Checking Guards"
    }

    def toSolutionFolder(s: SubmissionType): String = s match {
      case GuardSubmission => guardsDir
      case CheckSubmission => checkDir
    }

    def toScorePath(s: SubmissionType): String = s match {
      case GuardSubmission => "/" + scoreboardGuardsPath
      case CheckSubmission => "/" + scoreboardCheckPath
    }

  }

  import SubmissionType._


  val project_dir = System.getProperty("user.dir")
  val resource_path = project_dir + File.separator + "resources" + File.separator
  val team_names = "animals.abc"

  /* Structure of a Scenario instance

  - tasks
    - guards.pol  // file with polygons
    - check.pol   // file with polygon/guards
  - teams
    - [teamname]*
      - password
      - emails
  - guards
    - [teamname]*
      - results.num
      - dump.num
  - check
    - [teamname]*
      - results.num
      - dump.num
    */


  private def getTeamNames: Seq[String] = Source.fromFile(resource_path + team_names).getLines.toSeq

  // Effectful stuff
  def generatePassword(): String = new BigInteger(130, random).toString(32)

  def writeToNewFile(fpath: String, text: String): Unit = {
    val file = new File(fpath)
    if (file.exists()) {
      file.delete()
    }
    if (!file.exists()) {
      file.createNewFile()
    }
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(text)
    bw.close()
  }

  private def makeCredentialFiles(tDir: File): Unit = {
    val pFile = tDir.getAbsoluteFile + File.separator + passwordFile
    val eFile = tDir.getAbsoluteFile + File.separator + emailsFile
    writeToNewFile(pFile, generatePassword())
    writeToNewFile(eFile, "")
  }

  private def createTeamFolders(teamsPath: String, k: File => Unit): Unit = {
    for (team <- getTeamNames) {
      val tDir = new File(teamsPath + File.separator + team)
      val b = tDir.mkdirs()
      assert(b, s"Cannot create a folder for teacm $team in the path $teamsPath")
      k(tDir)
    }
  }

  def createResultsFolders(innerPath: String): Unit = {
    val gDir = new File(innerPath + File.separator + guardsDir)
    val b1 = gDir.mkdir()
    if (!b1) {
      println(s"Cannot create folder $gDir");
      return
    }

    // Create separate folders for teams in "guards"
    createTeamFolders(gDir.getAbsolutePath + File.separator, _ => ())

    val cDir = new File(innerPath + File.separator + checkDir)
    val b2 = cDir.mkdir()
    if (!b2) {
      println(s"Cannot create folder $cDir");
      return
    }

    // Create separate folders for teams in "check"
    createTeamFolders(cDir.getAbsolutePath + File.separator, _ => ())
  }

  def createTasksFolder(tasksDirPath: String): Unit = {
    val tasksDir = new File(tasksDirPath)
    if (tasksDir.exists()) {
      println(s"The directory $tasksDirPath already exists");
      return
    }
    tasksDir.mkdir()

    // Copy task files
    val gFile = new File(tasksDir.getAbsolutePath + File.separator + guardsTaskFile)
    try {
      val sgFile = new File(DEFAULT_GUARDS_FILE)
      io.FileUtils.copyFile(sgFile, gFile)
    } catch {
      case e: IOException =>
        e.printStackTrace()
        writeToNewFile(gFile.getAbsolutePath, "")
    }

    val cFile = new File(tasksDir.getAbsolutePath + File.separator + checkTaskFile)
    try {
      val scFile = new File(DEFAULT_CHECK_FILE)
      io.FileUtils.copyFile(scFile, cFile)
    } catch {
      case e: IOException =>
        e.printStackTrace()
        writeToNewFile(cFile.getAbsolutePath, "")
    }
  }

  def copyConfigFile(sRoot: String) = {
    val confFile = new File(sRoot + File.separator + configFile)
    try {
      val initConfFile = new File(DEFAULT_CONFIG_FILE)
      io.FileUtils.copyFile(initConfFile, confFile)
    } catch {
      case e: IOException =>
        e.printStackTrace()
        writeToNewFile(confFile.getAbsolutePath, "")
    }
  }

  // Create new scenario instance
  def populateInstance(basepath: String, name: String,
                       deleteOld: Boolean = false, testEmail: Option[String]): String = {
    val instancesDir = new File(basepath)
    if (!instancesDir.exists() || !instancesDir.isDirectory) {
      println("Cannot create a project here!")
      return ""
    }

    val sDir = new File(basepath + File.separator + name)
    if (sDir.exists() && !deleteOld) {
      println("Such scenario project already exists")
      return ""
    } else if (sDir.exists() && deleteOld) {
      deleteFolderRecursively(sDir)
    }

    val b = sDir.mkdir()
    if (!b) {
      println(s"Something is wrong here, cannot make directory $sDir")
      return ""
    }

    val innerPath = sDir.getAbsolutePath + File.separator
    createTasksFolder(innerPath + tasksDir)
    copyConfigFile(innerPath)
    createTeamFolders(innerPath + teamsDir, makeCredentialFiles)
    createResultsFolders(innerPath)

    sDir.getAbsolutePath
  }

  def deleteFolderRecursively(sDir: File): Try[(Int, Int)] = {
    import scalax.file.Path
    val path: Path = Path(sDir)
    Try(path.deleteRecursively(continueOnFailure = false))
  }

  /**
    * Check if string is a valid email
    */
  def isValidEmail(email: String): Boolean =
    """([\w\.-]+)@([\w\.]+)(\w+)""".r.unapplySeq(email).isDefined

  /*
   * Get a password for a team
   */
  def getTeamPassword(sDirPath: String, tName: String): Option[String] = {
    val teamPasswordPath = List(sDirPath, teamsDir, tName, passwordFile).mkString(File.separator)
    val pFile = new File(teamPasswordPath)
    if (!pFile.exists()) {
      println(s"Password file for the team  $tName should exist in the path $teamPasswordPath!")
      None
    } else {
      val lines = Source.fromFile(pFile).getLines.toSeq
      if (lines.isEmpty) {
        println(s"Password file for the team  $tName in path $teamPasswordPath is empty!")
        None
      } else {
        Some(lines.head.trim)
      }
    }
  }

  /*
   * Get a password for a team
   */
  def getTeamFeedback(sDirPath: String, tName: String): Option[String] = {
    val feedbackPath = List(sDirPath, teamsDir, tName, feedbackFile).mkString(File.separator)
    val pFile = new File(feedbackPath)
    if (!pFile.exists()) {
      println(s"Feedback file for the team  $tName should exist in the path $feedbackPath!")
      None
    } else {
      val lines = Source.fromFile(pFile).getLines.toSeq
      if (lines.isEmpty) {
        println(s"Password file for the team  $tName in path $feedbackPath is empty!")
        None
      } else {
        Some(lines.mkString("\n"))
      }
    }
  }


  def teamFolderExists(sDirPath: String, tName: String): Boolean = {
    val teamDirPath = List(sDirPath, teamsDir, tName).mkString(File.separator)
    val tFile = new File(teamDirPath)
    tFile.exists() && tFile.isDirectory
  }

  def getTeams(sDirPath: String): Seq[String] = {
    val teamDir = new File(List(sDirPath, teamsDir).mkString(File.separator))
    if (!teamDir.exists() || !teamDir.isDirectory) return Seq.empty
    teamDir.listFiles(new FileFilter {
      override def accept(f: File) = f.isDirectory && !f.getName.contains('.')
    }).toSeq.map(_.getName)
  }

  /*
   * Get a list of emails for the team
   */
  def getTeamEmails(sDirPath: String, tName: String): Seq[String] = {
    val teamEmailsPath = List(sDirPath, teamsDir, tName, emailsFile).mkString(File.separator)
    val eFile = new File(teamEmailsPath)
    if (!eFile.exists()) {
      println(s"Email file for the team  $tName should exist in the path $teamEmailsPath!")
      return Seq.empty
    }
    val fromFile = Source.fromFile(eFile).getLines
    val lines = fromFile.toIterator.toSeq
    val trs = for (l <- lines; trimmed = l.trim if trimmed.nonEmpty && isValidEmail(trimmed)) yield trimmed
    trs
  }

  def checkProjectStructure(dir: File): (Boolean, String) = {
    val subdirs = dir.listFiles(new FileFilter {
      override def accept(f: File) = f.isDirectory
    })
    val expected = Set(tasksDir, teamsDir, guardsDir, checkDir)
    val present = subdirs.map(_.getName).toSet
    val b1 = expected.subsetOf(present)
    if (!b1) {
      val diff = expected -- present
      return (false, s"The project doesn't have expected subfolders: $diff")
    }

    val tDir = new File(dir.getAbsolutePath + File.separator + tasksDir)
    if (!tDir.exists() || !tDir.isDirectory) {
      return (false, s"$tasksDir should exist and be a directory")
    }

    val taskFiles = tDir.listFiles().map(_.getName)
    if (!taskFiles.contains(guardsTaskFile)) {
      return (false, s"$tasksDir should contain the file $guardsTaskFile")
    }

    if (!taskFiles.contains(checkTaskFile)) {
      return (false, s"$tasksDir should contain the file $checkTaskFile")
    }

    (true, "OK")
  }

  ////////////////////////////////////////////////////////////////////////////////////////////
  ///// Guards-related getters
  ////////////////////////////////////////////////////////////////////////////////////////////


  def getGuardsPolygonMap(sDirPath: String): Map[Int, Polygon] = {
    val guardsFile = new File(List(sDirPath, tasksDir, guardsTaskFile).mkString(File.separator))
    if (!guardsFile.exists()) throw BadSetupException("No guards file found!")

    val polMap = try {
      Source.fromFile(guardsFile).getLines.toSeq.map(GuardInputParser(_).get).toMap
    } catch {
      case e: Throwable =>
        val msg = e.getMessage
        throw BadSetupException(s"Cannot parse the input: $msg")
    }
    polMap
  }

  def retrieveInputForGuards(sDirPath: String, pNum: Int): Either[Polygon, String] = {
    val polMap: Map[Int, Polygon] = getGuardsPolygonMap(sDirPath)
    if (polMap.contains(pNum)) Left(polMap(pNum))
    else Right(s"There is no polygon for the numer $pNum. Check your solution file.")
  }


  ////////////////////////////////////////////////////////////////////////////////////////////
  ///// Check-related getters
  ////////////////////////////////////////////////////////////////////////////////////////////


  def getCheckPolygonMap(sDirPath: String): Map[Int, (Polygon, Seq[Point2D])] = {
    val checkFile = new File(List(sDirPath, tasksDir, checkTaskFile).mkString(File.separator))
    if (!checkFile.exists()) throw BadSetupException("No guards file found!")

    val polMap = try {
      Source.fromFile(checkFile).getLines.toSeq.map(CheckInputParser(_).get).toMap
    } catch {
      case e: Throwable =>
        val msg = e.getMessage
        throw BadSetupException(s"Cannot parse the input: $msg")
    }
    polMap
  }

  def retrieveInputForCheck(sDirPath: String, pNum: Int): Either[(Polygon, Seq[Point2D]), String] = {
    val polMap: Map[Int, (Polygon, Seq[Point2D])] = getCheckPolygonMap(sDirPath)
    if (polMap.contains(pNum)) Left(polMap(pNum))
    else Right(s"There is no polygon for the numer $pNum. Check your solution file.")
  }

  ////////////////////////////////////////////////////////////////////////////////////////////
  ///// General machinery
  ////////////////////////////////////////////////////////////////////////////////////////////

  def sendResult(sDir: String, st: SubmissionType, tname: String, time: Date, body: String) {
    import EmailUtils._
    val subj = s"[$tname] Solution for ${toDescription(st)}"
    val tos = ProjectUtils.getTeamEmails(sDir, tname)
    val mails = for (tz <- tos) yield new Mail(to = tz, subject = subj, message = body)
    mails foreach send
  }

  // run at strartup
  def checkOrCreateSolutionFolders(sDirPath: String): Unit = {
    checkAndCreate(sDirPath, guardsDir)
    checkAndCreate(sDirPath, checkDir)
  }

  private def checkAndCreate(sDirPath: String, solDir: String): AnyVal = {
    val dir = new File(List(sDirPath, solDir).mkString(File.separator))
    if (dir.exists() && !dir.isDirectory) {
      dir.delete()
    }
    if (!dir.exists()) {
      dir.mkdir()
    }
  }
  def getTeamResultFolder(sDirPath: String, tname: String, st: SubmissionType): Option[File] = {
    val solDir = new File(List(sDirPath, toSolutionFolder(st), tname).mkString(File.separator))
    if (!solDir.exists()) {
      solDir.mkdir()
    }
    if (!solDir.exists() || !solDir.isDirectory) None
    else Some(solDir)
  }

  def getLastSolutionFileNumber(dir: File, prefix: String): (Option[String], Int) = {
    if (!dir.isDirectory) {
      throw new BadSetupException(s"$dir should be a directoryr")
    }

    val files = dir.listFiles(new FilenameFilter {
      override def accept(dir: File, name: String) = name.startsWith(prefix)
    }).toSeq

    val fNumbers: Seq[String] = files.map(_.getName.stripPrefix(prefix)).filter(_.trim.nonEmpty)

    if (fNumbers.isEmpty) return (None, 0)

    // This might throw an exception
    val nums = fNumbers.map(Integer.parseInt)
    val name_max = nums.foldLeft((None: Option[String], 0))((res, num) =>
      if (num > res._2) (Some(prefix + num), num) else res)
    name_max
  }


  private def dumpIntoFileAtomically(dir: File, prefix: String, content: String, tmpName: String, num: Int) = {
    val tmpPath = dir.getAbsolutePath + File.separator + tmpName
    val tmpFile = new File(tmpPath)
    writeToNewFile(tmpPath, content)
    val realName = prefix + num
    val realFile = new File(dir.getAbsolutePath + File.separator + realName)
    if (realFile.exists()) {
      throw new BadSetupException(s"File $realFile already exists!")
    }

    // Move atomically
    import java.nio.file.StandardCopyOption._
    Files.move(tmpFile.toPath, realFile.toPath, REPLACE_EXISTING, ATOMIC_MOVE)
  }

  def writeIntoResultFile(sDirPath: String, tName: String, st: SubmissionType, content: String, prefix: String): Unit = {
    val tfres = getTeamResultFolder(sDirPath, tName, st)
    if (tfres.isEmpty) {
      throw new BadSetupException(s"There is no team folder in $sDirPath for the team $tName")
    }
    val dir = tfres.get
    val num = getLastSolutionFileNumber(dir, prefix)._2 + 1
    // Dump and move
    dumpIntoFileAtomically(dir, prefix, content, prefix + tmpSuffix, num)
  }

}

case class BadSetupException(msg: String) extends Exception

/**
  * Creating a new scenario instance
  */
object SetupScenario {

  import ProjectUtils._

  /**
    * Create new scenario structure
    * args(0) -- base folder for the scenarios, e.g., ./instances
    * args(1) -- scenario name, e.g., test
    */
  def main(args: Array[String]) {
    if (args.length < 2) {
      println("Not enough arguments for creating a scenario structure.\n" +
          "Please, provide a base path and a scenario name.")
      return
    }

    val basePath = args(0)
    val scenarioName = args(1)

    var deleteOld: Boolean = false
    if (args.length >= 3) {
      deleteOld = java.lang.Boolean.parseBoolean(args(2))
    }

    var testEmail: Option[String] = None
    if (args.length >= 4) {
      testEmail = Some(args(3))
    }

    val res = populateInstance(basePath, scenarioName, deleteOld, testEmail)
  }

}

object MailPasswordsToEveryone {
  def main(args: Array[String]) {
    val sRoot = args(0)
    val tss = getTeamsPassEmails(sRoot)
    for ((tname, pass, emails) <- tss) {
      sendEmailWithPassword(tname, pass, emails)
    }
  }

  def sendEmailWithPassword(tname: String, pass: String, emails: Seq[String]): Unit = {
    if (emails.isEmpty) return
    val body =
      s"""Dear participant of the Scenario Week 4.
        |
        |You've been assigned to the team "$tname".
        |
        |Your team's password for submitting solutions is:
        |
        |$pass
        |
        |Good luck!
        |
        |Kind regards,
        |Scenario Week Team
      """.stripMargin

    val subj = s"[$tname] Welcome to Scenario Week 4"
    val mail = new Mail(to = emails, subject = subj, message = body)
//    EmailUtils.send(mail)
    println(s"Mailed to '$tname':\n${emails.mkString("\n")}")
    println()
  }

  def getTeamsPassEmails(sRoot: String): Seq[(String, String, Seq[String])] = {
    val ts = ProjectUtils.getTeams(sRoot)
    val res = for (tname <- ts) yield {
      val pass = ProjectUtils.getTeamPassword(sRoot, tname).get
      val emails = ProjectUtils.getTeamEmails(sRoot, tname).sorted
      (tname, pass, emails)
    }
    res
  }
}



object MailFeedbackToEveryone {
  def main(args: Array[String]) {
    val sRoot = args(0)
    val tss = getTeamsEmailsAndfeedback(sRoot)
    for ((tname, pass, emails) <- tss) {
      sendEmailWithFeedback(tname, pass, emails)
    }
  }

  def sendEmailWithFeedback(tname: String, text: String, emails: Seq[String]): Unit = {
    if (emails.isEmpty) return
    val body =
      s"""Dear participants of the Scenario Week 4.
        |
        |Please, find below the grades for the week and feedback on the implementation report for your team "$tname".
        |
        |$text
        |
        |Kind regards,
        |Ilya Sergey
      """.stripMargin

    val subj = s"[$tname] Grades and Feedback for Scenario Week 4"
    val mail = new Mail(to = emails, subject = subj, message = body, bcc=Seq("ilya.sergey@gmail.com"))
    EmailUtils.send(mail)
    println(s"Mailed to '$tname':\n${emails.mkString("\n")}")
    println()
  }

  def getTeamsEmailsAndfeedback(sRoot: String): Seq[(String, String, Seq[String])] = {
    val ts = ProjectUtils.getTeams(sRoot)
    val res = for (tname <- ts) yield {
      val feedback = ProjectUtils.getTeamFeedback(sRoot, tname).getOrElse("<No feedback>")
      val emails = ProjectUtils.getTeamEmails(sRoot, tname).sorted
      (tname, feedback, emails)
    }
    res
  }
}


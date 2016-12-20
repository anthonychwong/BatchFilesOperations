package ch3plusStudio.BatchFilesOperations

import java.io._
import java.nio.file.{ Files, Path, StandardCopyOption }

import scala.collection.JavaConversions._
import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.util.{ Try, Success, Failure }

import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._

//import converter.Extra._

/**
 * @author ${user.name}
 */
object App {

  implicit val formats = DefaultFormats

  def foo(x: Array[String]) = x.foldLeft("")((a, b) => a + b)

  def parseFreq(x: JValue): Try[Long] = Try((x \ "freq").extract[Long])

  def main(args: Array[String]) {
    // support of other config.json
    // println("concat arguments = " + foo(args))

    var isExit = false

    do {

      var big_jobj = parse(Source.fromFile("config.json").mkString)

      (big_jobj \ "operations").extract[List[JValue]].foreach(opt => {
        Try(new File((opt \ "folder").extract[String])) match {
          case Failure(thrown) => println(s"Failed to open the file with <$thrown>.")
          case Success(folder) => {
            if (!folder.exists()) {
              println(s"Folder <$folder> not exists.")
            } else if (!folder.isDirectory()) {
              println(s"Expecting a folder but a got a file <$folder>.")
            } else {
              for (
                file <- walkDir(
                  folder,
                  Try((opt \ "filefilter").extract[String]) getOrElse "(.*)",
                  Try((opt \ "subfolder").extract[Boolean]) getOrElse false,
                  Try((opt \ "subfolderfilter").extract[String]) getOrElse "(.*)")
              ) {
                try {
                  println(if (file.renameTo(new File((opt \ "para1").extract[String] + file.getName))) s"Successfully moved the file <$file>." else s"Failed to move the file <$file>.")
                } catch {
                  case e: Exception => println(s"Failed to move file <$file> with exception <$e>.")
                }
              }
            }
          }
        }
      })

      val writer = new PrintWriter(new File("config.json"))
      writer.write(pretty(big_jobj))
      writer.close()

      val nSleepTime: Long = Try((big_jobj \ "freq").extract[Long]) getOrElse -1

      if (nSleepTime > 0) {
        Thread.sleep(nSleepTime)
      } else {
        isExit = true
      }

    } while (!isExit)
  }

  def walkDir(dir: File, fileFilter: String, inclSubFolder: Boolean, folderFilter: String): Array[File] = {
    dir.listFiles().filter { f => (f.exists()) & ((inclSubFolder & f.isDirectory() & f.getName.matches(folderFilter)) | (f.isFile() & f.getName.matches(fileFilter))) }
      .flatMap((f: File) => if (f.isDirectory()) walkDir(f, fileFilter, inclSubFolder, folderFilter) else Array(f))
  }
}

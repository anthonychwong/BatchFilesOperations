package ch3plusStudio.BatchFilesOperations

import java.io.{ File, PrintWriter };

import java.nio.file.{ Paths, Path, Files, StandardCopyOption }

import net.lingala.zip4j.core.ZipFile
import net.lingala.zip4j.model.ZipParameters
import net.lingala.zip4j.util.Zip4jConstants

import java.util.zip.{ ZipEntry, ZipOutputStream };

import scala.io.Source
import scala.util.{ Failure, Success, Try }

import org.json4s.DefaultFormats
import org.json4s.JValue
import org.json4s.jackson.JsonMethods.parse
import org.json4s.jackson.JsonMethods.pretty
import org.json4s.jvalue2extractable
import org.json4s.jvalue2monadic
import org.json4s.string2JsonInput

import scalaz._
import scalaz.Validation
import scalaz.Scalaz._

/**
 * @author ${anthony.wong}
 */

object App {

  implicit val formats = DefaultFormats

  def foo(x: Array[String]) = x.foldLeft("")((a, b) => a + b)

  def main(args: Array[String]) {
    // support of other config.json
    // println("concat arguments = " + foo(args))

    var isExit = false

    do {

      var big_jobj = parse(Source.fromFile("config.json").mkString)

      (big_jobj \ "operations").extract[List[JValue]].foreach(opt => {

        validate(opt).toEither match {
          case Left(errors) => for (error <- errors) println(error)
          case Right(result) => {

            val folder = new File((opt \ "folder").extract[String])

            val files = walkDir(
              folder,
              Try((opt \ "filefilter").extract[String]) getOrElse "(.*)",
              Try((opt \ "subfolder").extract[Boolean]) getOrElse false,
              Try((opt \ "subfolderfilter").extract[String]) getOrElse "(.*)")

            (opt \ "operation").extract[String] match {
              case "move" => {
                for (file <- files) {
                  println(Try(file.renameTo(new File((opt \ "para1").extract[String] + file.getName))) match {
                    case Success(r) => if (r) s"Successfully moved the file <$file>." else s"Failed to move the file <$file>."
                    case Failure(t) => s"Failed to move file <$file> with exception <$t>."
                  })
                }
              }
              case "delete" => {
                for (file <- files) {
                  println(Try(file.delete) match {
                    case Success(r) => if (r) s"Successfully deleted the file <$file>." else s"Failed to delete the file <$file>."
                    case Failure(t) => s"Failed to delete file <$file> with exception <$t>."
                  })
                }
              }
              case "compress" => {

                var pDest = Paths.get((opt \ "para1").extract[String])
                var pOrgBak = Paths.get(pDest.getParent.toString(), "org_" + pDest.getFileName)
                var pTmp = Paths.get(pDest.getParent.toString(), "tmp_" + pDest.getFileName)

                def canWriteIntoFile(f: File): Boolean =
                  if (!f.exists()) {
                    if (f.createNewFile())
                      f.delete()
                    else
                      false
                  } else {
                    if (f.canRead() && f.canWrite())
                      f.delete()
                    else
                      false
                  }

                TryCatchFinalCatchChain[Unit](List(

                  (
                    // 1. Test can write into backup
                    Unit =>
                      if (!(canWriteIntoFile(pOrgBak.toFile())))
                        throw new Throwable(s"Failed to write into <$pOrgBak>"),
                    (t: Throwable) => {
                      println(s"Failed to create backup file with following error\n$t")
                      t
                    },
                    Unit => {},
                    (t: Throwable) => t),

                  (
                    // 2. Test can write into temp
                    Unit =>
                      if (!(canWriteIntoFile(pTmp.toFile())))
                        throw new Throwable(s"Failed to write into <$pTmp>"),
                    (t: Throwable) => {
                      println(s"Failed to create zip file with following error\n$t")
                      t
                    },
                    Unit => {},
                    (t: Throwable) => t),

                  (
                    // 3. Write into temp
                    Unit => {
                      var zipFile = new ZipFile(pTmp.toFile())

                      for (file <- files) {
                        var zipPara = new ZipParameters()
                        zipPara.setCompressionMethod(Zip4jConstants.COMP_DEFLATE)
                        zipPara.setFileNameInZip(folder.getParentFile.toURI().relativize(file.toURI()).getPath)
                        zipPara.setRootFolderInZip(folder.getParentFile.toURI().relativize(file.getParentFile.toURI()).getPath)

                        zipFile.addFile(file, zipPara)
                      }
                    },
                    (t: Throwable) => {
                      println(s"Failed to write into zip file with following error \n $t")

                      println(Try(Files.delete(pTmp)) match {
                        case Success(r) => if (r != null) s"Successfully deleted the file <$pTmp>." else s"Failed to delete the file <$pTmp>."
                        case Failure(t) => s"Failed to delete file <$pTmp> with exception <$t>."
                      })

                      t
                    },
                    Unit => {},
                    (t: Throwable) => t),

                  (
                    // 4. backup the original
                    Unit => {
                      if (pDest.toFile().exists())
                        Files.move(pDest, pOrgBak, StandardCopyOption.REPLACE_EXISTING)
                    },
                    (t: Throwable) => {
                      println(s"New zip file created successfully as <$pTmp> but manual action required because failed to backup original with following exception.\n$t")
                      t
                    },
                    Unit => {},
                    (t: Throwable) => t),

                  (
                    // 5. rename temp to new version of backup
                    Unit => {
                      Files.move(pTmp, pDest, StandardCopyOption.REPLACE_EXISTING)
                    },
                    (t: Throwable) => {
                      println(s"New zip file created successfully as <$pTmp> but manual action required because failed to rename it with following exception.\n$t")
                      t
                    },
                    Unit => {},
                    (t: Throwable) => t),

                  (
                    // 6. delete original
                    Unit => {
                      if (!pOrgBak.toFile().delete())
                        throw new Throwable(s"Can't delete backup file <$pOrgBak>.")
                    },
                    (t: Throwable) => {
                      println(s"New zip file created successfully as <$pDest> but manual action required because failed to delete the backup file with following exception.\n$t")
                      t
                    },
                    Unit => {},
                    (t: Throwable) => t)))
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

  def validate(jValue: JValue): Validation[NonEmptyList[Throwable], JValue] = {

    def validateFileObjWithActions(jValue: JValue, key: String, actions: ((File) => Validation[NonEmptyList[Throwable], JValue])*): Validation[NonEmptyList[Throwable], JValue] = {
      Try(new File((jValue \ key).extract[String])) match {
        case Failure(thrown) => thrown.failureNel
        case Success(f)      => (for (action <- actions) yield action(f)).reduceLeft((l, r) => (l |@| r)((_, _) => jValue))
      }
    }

    def canWrite(obj: File): Validation[NonEmptyList[Throwable], JValue] =
      if (obj.canWrite()) jValue.success else (new Throwable(s"Permission to write to <$obj> is required.")).failureNel[JValue]

    def canRead(obj: File): Validation[NonEmptyList[Throwable], JValue] =
      if (obj.canRead()) jValue.success else (new Throwable(s"Permission to read <$obj> is required.")).failureNel[JValue]

    def isExists(obj: File): Validation[NonEmptyList[Throwable], JValue] =
      if (obj.exists()) jValue.success else new Throwable(s"Path <$obj> does not exists.").failureNel[JValue]

    def isFolder(obj: File): Validation[NonEmptyList[Throwable], JValue] =
      if (obj.isDirectory()) jValue.success else new Throwable(s"Expecting a folder but got a file <$obj>.").failureNel[JValue]

    def isFile(obj: File): Validation[NonEmptyList[Throwable], JValue] =
      if (obj.isFile()) jValue.success else new Throwable(s"Expecting a file but a got a folder <$obj>.").failureNel[JValue]

    def canCreateEmptyOrReadWriteExistingFile(obj: File): Validation[NonEmptyList[Throwable], JValue] = {
      try {
        if (if (!obj.exists()) obj.createNewFile() else obj.isFile() && obj.canRead() && obj.canWrite()) jValue.success else new Throwable(s"Can't create or write into file <$obj>.").failureNel[JValue]
      } catch {
        case t: Throwable => t.failureNel[JValue]
      }
    }

    def validateActionMove(jValue: JValue): Validation[NonEmptyList[Throwable], JValue] = {
      (validateFileObjWithActions(jValue, "folder", isExists _, isFolder _, canWrite _, canRead _)
        |@| validateFileObjWithActions(jValue, "para1", isExists _, isFolder _, canWrite _, canRead _))((_, _) => jValue)
    }

    def validateActionCompress(jValue: JValue): Validation[NonEmptyList[Throwable], JValue] = {
      (validateFileObjWithActions(jValue, "folder", isExists _, isFolder _, canWrite _, canRead _)
        |@| validateFileObjWithActions(jValue, "para1", canCreateEmptyOrReadWriteExistingFile _, canWrite _, canRead _))((_, _) => jValue)
    }

    Try((jValue \ "operation").extract[String]) match {
      case Failure(thrown) => thrown.failureNel[JValue]
      case Success(action) => {
        action match {
          case "move"     => validateActionMove(jValue)
          case "compress" => validateActionCompress(jValue)
          case "delete"   => validateFileObjWithActions(jValue, "folder", isExists _, isFolder _, canWrite _, canRead _)
          case _          => new Throwable(s"Action <$action> is not supported").failureNel
        }
      }
    }
  }

  def TryCatchFinalCatchChain[R](actions: List[(Unit => R, Throwable => Throwable, Unit => Unit, Throwable => Throwable)]): List[(List[Throwable], Option[R])] = {
    var lResult: List[(List[Throwable], Option[R])] = Nil

    for ((tryAction, catchAction, finalAction, catchFinalAction) <- actions) {
      if (lResult.isEmpty || lResult.last._1.isEmpty) {

        val (lT, lR) = List({
          Try(tryAction()) match {
            case Success(r) => (None, Some(r))
            case Failure(t) => (Some(catchAction(t)), None)
          }
        }, {
          Try(finalAction()) match {
            case Success(_) => (None, None)
            case Failure(t) => (Some(catchFinalAction(t)), None)
          }
        }).unzip

        lResult = lResult :+ (lT.flatten, lR.head)
      }
    }

    lResult
  }
}

package adv

import scala.io.Source
import java.nio.file.{Paths, Files, FileSystems}
import scala.collection.JavaConverters._
import java.nio.charset.StandardCharsets
import java.io.BufferedWriter
import java.io.FileWriter
import java.io.File

val resourcesPath = "./src/main/resources/"
val solutionsPath = "./src/main/scala/"
val readmePath    = "./README.md"

def readFromFile(filename: String): List[String] =
  val path = resourcesPath + filename
  val file = Source.fromFile(path)
  val it = for line <- file.getLines() yield line
  val lines = it.toList
  file.close()
  lines

def readLineFromFile(filename: String): String = 
  val path = resourcesPath + filename
  val file = Source.fromFile(path)
  val contents = file.getLines().mkString
  file.close()
  contents

def generateReadme(): Unit =
  val solutionsDir = FileSystems.getDefault().getPath(solutionsPath)
  val filenames = 
    Files.list(solutionsDir).iterator().asScala.toList
    .map(_.getFileName().toString().takeWhile(_ != '.'))
  val puzzles = filenames.filter(_.forall(Character.isDigit))
  val dates = puzzles.map(_.toInt).filter(_ > 0)

  // Delete previous README and generate a new one
  try 
    val readme = File(readmePath)
    readme.delete()
    readme.createNewFile()

    val bw = BufferedWriter(FileWriter(readme, true))

    bw.write("## Advent Of Code 2023\n\n")
    bw.write("### Advent Of Code 2023, solved in Scala 3\n\n")

    bw.write("|  + | 1 | 2 | 3 | 4 | 5 |\n")
    bw.write("|:--:|:-:|:-:|:-:|:-:|:-:|\n")

    for i <- 0 until 5 do
      bw.write(s"| ${"%02d".format(5*i)} |")
      for j <- 1 to 5 do
          val solutionNo = 5*i + j
          val solutionFile = "%02d".format(solutionNo) + ".worksheet.sc"
          val linkToSolution = s"[☑](https://github.com/kotleta2007/advent-of-code-2023/tree/main/src/main/scala/${solutionFile})"
          val linkOrNone = if dates.contains(solutionNo) then linkToSolution else ""
          bw.write(s" ${linkOrNone} |")
      bw.write("\n")

    bw.close
  catch
    case e: Exception => e.printStackTrace()
  finally
    println("README generated.") 

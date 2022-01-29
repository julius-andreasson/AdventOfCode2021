package aoc
object util {
  def readLines(fileName: String): Array[String] =
    scala.io.Source.fromFile(fileName).getLines.toArray
}

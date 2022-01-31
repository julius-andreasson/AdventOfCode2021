package aoc

object util {
  def readLines(fileName: String): Array[String] =
    scala.io.Source.fromFile(fileName).getLines.toArray
  
  def deepClone(input: Array[Array[Int]]): Array[Array[Int]] = 
    (for i <- input.indices yield input(i).clone).toArray
}

import scala.io.Source

object d02: 
  // @main
  def day2(): Unit =
    val input = Source.fromFile("input/2").getLines.toArray
    println(s"Part 1: ${day2part1(input)}")
    println(s"Part 2: ${day2part2(input)}")

  def day2part1(input: Array[String]): String =
    var depth = 0;
    var forward = 0;
    input
      .map(str => str.split(" "))
      .map(s => (s(0), s(1).toInt))
      .foreach(s =>
        s._1 match
          case "forward"  => forward += s._2
          case "down"     => depth += s._2
          case "up"       => depth -= s._2
      )
    (forward * depth).toString

  def day2part2(input: Array[String]): String =
    var depth = 0
    var forward = 0
    var aim = 0
    input
      .map(str => str.split(" "))
      .map(s => (s(0), s(1).toInt))
      .foreach(s =>
        s._1 match
          case "forward"  => 
            forward += s._2
            depth += aim * s._2
          case "down"     => aim += s._2
          case "up"       => aim -= s._2
      )
    (forward * depth).toString

package aoc

object d02:
  def run(): Unit =
    val input = util.readLines("input/2")
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")

  def part1(input: Array[String]): String =
    var depth = 0
    var forward = 0
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

  def part2(input: Array[String]): String =
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

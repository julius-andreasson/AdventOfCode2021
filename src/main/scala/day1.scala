package aoc

object d01:
  // @main
  def day1(): Unit =
    val input = util.readLines("input/1").toArray.map(_.toInt)
    println(s"Part 1: ${day1f(input)}") // 1688
    println(s"Part 2: ${day1f(input.sliding(3).toArray.map(_.sum))}") // 1728

  def day1f(input: Array[Int]): Int =
    input.indices.tail.count(i => input(i) > input(i - 1))
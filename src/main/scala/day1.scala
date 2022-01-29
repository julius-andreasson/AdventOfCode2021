package aoc

object d01:
  def run(): Unit =
    val input = util.readLines("input/1").map(_.toInt)
    println(s"Part 1: ${func(input)}")
    println(s"Part 2: ${func(input.sliding(3).toArray.map(_.sum))}")

  def func(input: Array[Int]): Int =
    input.indices.tail.count(i => input(i) > input(i - 1))
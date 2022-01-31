package aoc

object d07:
  def run(): Unit =
    val crabs: Array[Int] = 
      util.readLines("input/7").head.split(",").toArray.map(_.toInt)
    func(crabs.clone, false)
    func(crabs.clone, true)

  def sumproduct(input: Array[Int], x: Int): Int =
    input.map(i => math.abs(i - x)).sum

  def weightedSumproduct(input: Array[Int], x: Int): Int =
    input.map(i => 
      (1 to math.abs(i - x)).sum
    ).sum

  def weight(x: Int): Int = (1 to x).sum

  def func(input: Array[Int], weighted: Boolean): Unit =
    var min = Int.MaxValue
    var minX = -1
    for x <- input.min.toInt until input.max.toInt do
      val sum = if weighted then weightedSumproduct(input, x) else sumproduct(input, x)
      if sum < min then
        min = sum
        minX = x
    println(s"$minX, $min")

package aoc

object d06:
  def run(): Unit =
    val fishPopulation: Array[Long] = Array.fill[Long](9)(0)
    util.readLines("input/6").head.split(",").foreach(s => fishPopulation(s.toInt) += 1)
    func(fishPopulation.clone, 80)
    func(fishPopulation.clone, 256)

  def func(input: Array[Long], days: Int): Unit =
    for day <- 0 until days do
      val oldFish = input(0)
      for t <- 0 until 8 do 
        input(t) = input(t + 1)
      input(6) += oldFish
      input(8) = oldFish
    println(input.sum.toString)

// @main
def day6(): Unit =
  val fishPopulation: Array[Long] = Array.fill[Long](9)(0)
  scala.io.Source.fromFile("input/6").getLines.next.split(",").foreach(s => fishPopulation(s.toInt) += 1)
  day6f(fishPopulation.clone, 80)
  day6f(fishPopulation.clone, 256)

def day6f(input: Array[Long], days: Int): Unit =
  for day <- 0 until days do
    val oldFish = input(0)
    for t <- 0 until 8 do 
      input(t) = input(t + 1)
    input(6) += oldFish
    input(8) = oldFish
  println(input.sum.toString)

package aoc

object d13:
  import scala.collection.mutable.{Map, Set, Queue}

  val input: Array[(Int, Int)] = 
    util.readLines("input/13").filter(str => str(0) != 'f').map(_.split(",")).map(p => (p(0).toInt, p(1).toInt))
  val folds: Array[(Boolean, Int)] =
    util.readLines("input/13").filter(str => str(0) == 'f').map(str => str.drop(11)).map(str => str.filter(c => c != '='))
    .map(str => 
      ( (str.take(1) == "x"), str.drop(1).toInt )
    )

  def run(): Unit =
    part1()
    part2()

  def part1(): Unit =
    val x_fold = 655 // x-coordinate at which to fold the paper. 
    val set: Set[(Int, Int)] = Set.empty[(Int,Int)]
    for i <- input do
      if i._1 > x_fold then
        set += ((2 * x_fold - i._1, i._2))
      else
        set += (i)
    println(s"Number of points after first fold: ${set.size}")

  def part2(): Unit =
    var set: Set[(Int, Int)] = input.to(Set)
    for fold <- folds do
      val newSet = Set.empty[(Int, Int)]
      for i <- set do
        if fold._1 then
          if i._1 > fold._2 then
            newSet += ((2 * fold._2 - i._1, i._2))
          else
            newSet += (i)
        else
          if i._2 > fold._2 then
            newSet += ((i._1, 2 * fold._2 - i._2))
          else
            newSet += (i)
      end for
      set = newSet
    end for

    println("\nSecret code:\n")
    for j <- 0 to 6 do
      for i <- 0 to 40 do
        print(if set.contains((i, j)) then "|" else " ")
      println

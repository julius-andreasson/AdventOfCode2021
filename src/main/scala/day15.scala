package aoc

object d15:
  import scala.collection.mutable.{Map, Queue, PriorityQueue}  

  def run(): Unit =
    val input_part1: Array[Array[Int]] = 
      util.readLines("input/15").map(_.map(_.toString.toInt).toArray)
    func(input_part1)
    func(processInput(input_part1))
  
  def getSurrounding(i: Int, j: Int, input: Array[Array[Int]]): Vector[(Int, Int)] =
    for
      p <- Vector((i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1))
      if p._1 >= 0 && p._1 < input.length && p._2 >= 0 && p._2 < input(0).length
    yield
      p
  
  def func(input: Array[Array[Int]]): Unit =
    val goal: (Int, Int) = (input.length - 1, input(0).length - 1)
    
    val frontier: PriorityQueue[(Int, Int, Int)] = PriorityQueue()(Ordering.by(p => -p._3))
    frontier.addOne((0, 0, 0))
    
    val came_from: Map[(Int, Int), (Int, Int)] = Map.empty[(Int, Int), (Int, Int)]
    came_from.addOne(((0, 0) -> (-1, -1)))

    val cost_so_far: Map[(Int, Int), Int] = Map.empty[(Int, Int), Int]
    cost_so_far.addOne(((0, 0) -> 0))
    
    var break = false
    while !break && !frontier.isEmpty do
      val temp = frontier.dequeue
      val current = (temp._1, temp._2)
      
      break = current._1 == goal._1 && current._2 == goal._2

      for neighbor <- getSurrounding(current._1, current._2, input) do
        val new_cost = (cost_so_far(current) + input(neighbor._1)(neighbor._2))
        if !cost_so_far.contains(neighbor) || new_cost < cost_so_far(neighbor) then
          cost_so_far(neighbor) = new_cost
          val neighborP = (neighbor._1, neighbor._2, new_cost)
          frontier.enqueue(neighborP)
          came_from.addOne(neighbor -> current)
    
    println(cost_so_far(goal))

  def processInput(input: Array[Array[Int]]): Array[Array[Int]] = 
    val xMax = input.length
    val yMax = input(0).length
    val res: Array[Array[Int]] = Array.fill[Int](xMax * 5, yMax * 5)(0)
    for i <- res.indices; j <- res(0).indices do
      val x = input(i % xMax)(j % yMax) + i / xMax + j / yMax
      res(i)(j) = if x > 9 then x % 10 + 1 else x
    res

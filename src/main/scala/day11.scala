package aoc

import scala.collection.mutable.Queue

/* TODO
> input into octopi: Array[Array[Int]]
> hasFlashed Array[Array[Boolean]]
> getSurrounding(x: Int, y: Int): Array[(Int, Int)]
> for i, j do {octopi(i)(j) += 1} // increment all by 1
> for i, j do {if !hasFlashed && octopi(i)(j) > 9 then getSurrounding.foreach(p => octopi(p._1)(p._2) += 1)}
> for i, j do {if hasFlashed then octopi(i)(j) = 0} // reset all that flashed
*/

/* TODO 2: Queue Boogaloo
input into octopi: Array[Array[Int]]
def getSurrounding(x: Int, y: Int): Array[(Int, Int)]
var flashes = 0L
for iteration <- (1 to iterations) do
  val canFlashQueue: Queue (Int, Int) = for i <- octopi; j <- octopi(i) yield (i, j)
  for i, j do {octopi(i)(j) += 1} // increment all by 1
  while(!q.isEmpty) do {if && octopi(i)(j) > 9 then getSurrounding.foreach(p => octopi(p._1)(p._2) += 1)}
  for i, j do {if hasFlashed then flashes += 1; octopi(i)(j) = 0} // reset all that flashed
*/

object d11:
  val input: Array[Array[Int]] = 
    util.readLines("input/11").toArray.map(_.map(_.toString.toInt).toArray)

  // @main
  def day11(): Unit =
    day11part1(input.clone)
    // day11part2(input.clone)
  
  def getSurrounding(i: Int, j: Int): IndexedSeq[(Int, Int)] =
    for 
      dx <- -1 to 1
      dy <- -1 to 1
      if i + dx >= 0 && i + dx < input.length && j + dy >= 0 && j + dy < input(0).length
    yield
      (i + dx, j + dy)
  
  def day11part1(octopi: Array[Array[Int]]): Unit =
    var flashes = 0L
    for iteration <- (1 to 100) do
      val hasFlashed: Array[Array[Boolean]] = Array.fill[Boolean](octopi.length, octopi(0).length)(false)
      
      for i <- octopi.indices; j <- octopi(i).indices do octopi(i)(j) += 1 // increment all by 1
      
      var newFlashes = true
      while newFlashes do
        newFlashes = false
        for i <- octopi.indices; j <- octopi(i).indices do 
          if !hasFlashed(i)(j) && octopi(i)(j) > 9 then 
            hasFlashed(i)(j) = true
            getSurrounding(i, j).foreach(p => octopi(p._1)(p._2) += 1)
            newFlashes = true
      end while  

      flashes += hasFlashed.flatten.count(b => b)
      if hasFlashed.flatten.filter(b => b).length == octopi.flatten.length then println(s"ALL $iteration")
      for i <- octopi.indices; j <- octopi(i).indices do 
        if hasFlashed(i)(j) then octopi(i)(j) = 0 // reset all that flashed
      
    println(flashes)
    
  def day11part2(): Unit =
    println("wow2")

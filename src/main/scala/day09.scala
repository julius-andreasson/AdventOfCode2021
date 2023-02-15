package aoc

object d9:
  import scala.collection.mutable.{Map, Set}

  val input: Array[Array[Int]] = util.readLines("input/9").map(_.map(_.toString.toInt).toArray)
  
  def run(): Unit =
    part1()
    part2()

  def checkLowest(i: Int, j: Int): Boolean =
    !Vector((i + 1, j), (i - 1, j), (i, j + 1), (i, j - 1))
      .filterNot(p => p._1 < 0 || p._1 >= input.length || p._2 < 0 || p._2 >= input(0).length)
      .map(p => input(p._1)(p._2) >= input(i)(j))
      .contains(false)

  def addLowerPoints(i: Int, j: Int, set: Set[(Int, Int)]): Unit =
    Vector((i + 1, j), (i - 1, j), (i, j + 1), (i, j - 1))
      .filterNot(p => p._1 < 0 || p._1 >= input.length || p._2 < 0 || p._2 >= input(0).length)
      .filter(p => input(p._1)(p._2) != 9)
      .foreach(p => set += p)

  def part1(): Unit =
    val lowPoints: IndexedSeq[Int] = 
      (for i <- input.indices yield
        for j <- input(i).indices if checkLowest(i, j) yield
          input(i)(j)
      ).flatten
    println(lowPoints.sum + lowPoints.length)

  def basinSize(startingPoint: (Int, Int)): Int =
    var oldSet: Set[(Int, Int)]= Set.empty[(Int, Int)]
    var newSet: Set[(Int, Int)]= Set.empty[(Int, Int)]
    // initialize newSet
    newSet += startingPoint
    while !oldSet.equals(newSet) do
      oldSet = newSet.clone
      for point <- newSet do 
        addLowerPoints(point._1, point._2, newSet)
    end while
    newSet.size

  def part2(): Unit =
    val lowPoints: IndexedSeq[(Int, Int)] = 
      (for i <- input.indices yield
        for j <- input(i).indices if checkLowest(i, j) yield
          (i, j)
      ).flatten
    var basinSizes: Vector[Int] = Vector()
    for startingPoint <- lowPoints do
      basinSizes = basinSizes :+ basinSize(startingPoint)
    println(basinSizes.sorted.reverse.take(3).product)
    
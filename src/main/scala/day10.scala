package aoc

object d10:
  import scala.collection.mutable.Stack
  val input = util.readLines("input/10")
  var filterIndexes: Vector[Int] = Vector.empty[Int]

  def run(): Unit =
    part1()
    part2()

  def part1(): Unit =
    var illegalVector: Vector[Char] = Vector.empty[Char]
    for i <- input.indices do
      val stack: Stack[Char] = Stack.empty[Char]
      var found = false
      var firstIllegal: Char = ' '
      for c <- input(i) if !found do
        if starts.contains(c) then
          stack.prepend(c)
        else
          if matching(stack.top, c) then
            stack.pop
          else
            firstIllegal = c
            found = true
      end for      
      if found then 
        illegalVector = illegalVector :+ firstIllegal
        filterIndexes = filterIndexes :+ i // helps part 2 out
    end for
    val points: Vector[Int]   = Vector(3, 57, 1197, 25137)
    val point = illegalVector.map(c => points(ends.indexOf(c)))
    println(point.sum)
    
  def part2(): Unit =
    val part2input = 
      (for i <- input.indices if !filterIndexes.contains(i) yield
        input(i)
      ).toArray
    var totalPoints = Vector.empty[Long]
    for i <- part2input.indices do
      val stack: Stack[Char] = Stack.empty[Char]
      var found = false
      var closingSeq: Vector[Char] = Vector.empty[Char]
      for c <- part2input(i) if !found do
        if starts.contains(c) then
          stack.prepend(c)
        else
          if matching(stack.top, c) then
            stack.pop
      end for
      var seqPoints = 0L
      while !stack.isEmpty do
        seqPoints *= 5
        seqPoints += starts.indexOf(stack.pop) + 1
      totalPoints = totalPoints :+ seqPoints
    end for
    println(totalPoints.sorted.apply(totalPoints.length / 2))
    // println(totalPoints.sorted.mkString("\n"))

  def matching(start: Char, end: Char): Boolean = 
      starts.indexOf(start) == ends.indexOf(end)

  val starts = "([{<"
  val ends = ")]}>"

import scala.io.Source

case class Point(y: Int, x: Int){
}
// @main
def day5(): Unit =
  val input: Array[String] = Source.fromFile("input/5").getLines.toArray
  val arr: Array[Array[Point]] = 
    (for i <- input.indices yield 
      input(i)
        .split(" -> ")
        .map(str => 
          Point(
            x = str.split(",")(0).toInt, 
            y = str.split(",")(1).toInt)
        )
    )
    .toArray
  println(day5part2(arr))

def setLine(map: Array[Array[Int]], p1: Point, p2: Point, doDiagonals: Boolean): Unit =
  val dx = p2.x - p1.x
  val dy = p2.y - p1.y
  if dy == 0 then
    xOnly(map, p1, dx)
  else if dx == 0 then
    yOnly(map, p1, dy)
  else if doDiagonals then
    diagonal(map, p1, dx, dy)

def diagonal(map: Array[Array[Int]], p1: Point, dx: Int, dy: Int): Unit =
  val xDir = if dx < 0 then -1 else 1
  val yDir = if dy < 0 then -1 else 1
  for i <- 0 to math.abs(dx) do
    map(p1.x + i * xDir)(p1.y + i * yDir) += 1

def xOnly(map: Array[Array[Int]], p1: Point, dx: Int): Unit =
  val xDir = if dx < 0 then -1 else 1
    for x <- 0 to dx by xDir do
      map(p1.x + x)(p1.y) += 1

def yOnly(map: Array[Array[Int]], p1: Point, dy: Int): Unit =
  val yDir = if dy < 0 then -1 else 1
    for y <- 0 to dy by yDir do
      map(p1.x)(p1.y + y) += 1

def printMap(map: Array[Array[Int]]): Unit =
  for i <- map.indices do
    println(map(i).mkString(""))
  println()

def day5part1(arr: Array[Array[Point]]): Int =
  val map: Array[Array[Int]] = Array.fill[Int](991, 990)(0)
  for i <- arr.indices do
    setLine(map, arr(i)(0), arr(i)(1), false)
  map.flatten.count(_ >= 2)

def day5part2(arr: Array[Array[Point]]): Int =
  val map: Array[Array[Int]] = Array.fill[Int](991, 990)(0)
  for i <- arr.indices do
    setLine(map, arr(i)(0), arr(i)(1), true)
  map.flatten.count(_ >= 2)
  
package aoc

import scala.collection.mutable.{Map, Queue, PriorityQueue}

object d16:

  // @main
  def day16(): Unit =
    val input: Vector[Boolean] = hexToBin(util.readLines("input/16a")(0)).toVector
    day16part1(input)
    // day16part1(processInput(input_part1))

  def hexToBin(str: String): Array[Boolean] =
    var res = ""
    str.foreach(c => 
      var n = BigInt(c.toString, 16).toString(2)
      while n.length < 4 do
        n = n.prepended('0')
      res += n
      // println(s"$c, $n")
      )
    res.map(_ == '1').toArray

  def binaryToDecimal(input: Vector[Boolean]): Int =
    var res = 0
    for i <- input.indices do
      val revIn = input.reverse
      if revIn(i) then
        res += 1 << i
    res

  extension (vec: Vector[Boolean])
    def str: String =
      vec.map(b => if b then '1' else '0').mkString("")
  
    def toDecimal: Int =
      var res = 0
      for i <- vec.indices do
        val revIn = vec.reverse
        if revIn(i) then
          res += 1 << i
      res

  extension (it: Iterator[Boolean])
    def nextN(n: Int): Vector[Boolean] =
      (for i <- 1 to n yield
        it.next).toVector

  def getVersionSum(i: Int): Int =
    ???

  def day16part1(input: Vector[Boolean]): Unit =
    // Use iterator[Boolean]? 
    /*
    3 packet version
    3 packet type ID
    5 sub repeat (until one starts with 0)
    */
    var versions: Vector[Int] = Vector()
    var i = 0
    while i < input.length && input.takeRight(input.length - i).contains(true) do
      val version: Int = input.slice(i, i + 3).toDecimal // save packet version
      i += 3
      val ID = input.slice(i, i + 3).toDecimal // Save packet ID
      i += 3
      var length = -1
      var ltype = input(i)
      i += 1
      if ltype then
        // length type 1
        val n = input.slice(i, i + 11).toDecimal
        i += 11
        // contains sub-packages!
      else
        // length type 0
        length = input.slice(i, i + 15).toDecimal
        i += 15
      versions = versions :+ version
      println(s"version ${version}")
      i += length // skip rest of package

    // println(input)
    println("wow1")

  def day16part2(): Unit =
    println("wow2")
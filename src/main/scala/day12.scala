package aoc

import scala.collection.mutable.{Map, Set, Queue}

object d12:

  val input: Array[(String, String)] = 
    util.readLines("input/12a").toArray.map(_.split("-")).map(p => (p(0), p(1)))

  def nextPoints(str: String): Array[String] =
    input.filter(p => p._1 == str).map(p => p._2)

  // @main
  def day12(): Unit =
    day12part1()
    // day12part2(input.clone)
  
  def day12part1(): Unit =
    val frontier: Queue[String] = Queue("start")
    val came_from: Set[(String, String)] = Set(("start", "A"))
    
    while !frontier.isEmpty do
      val current = frontier.dequeue
      for neighbor <- nextPoints(current) do
        val pair = (neighbor, current)
        if !came_from.contains(pair) then
          frontier.enqueue(neighbor)
          came_from.addOne(pair)
    println(came_from.mkString("\n"))
    
    /*
    for option <- options do
      if path.contains(option) && option == option.toLowerCase then

    */
    

    // val frontier2: Queue[String] = Queue("start")
    // val reached2: Set[Set[String]] = Set.empty[Set[String]]
    
    // while !frontier2.isEmpty do
    //   val current = frontier2.dequeue
    //   for neighbor <- nextPoints(current) do
        // println(s"DURING: $neighbor -> $current")
        // val pair = (neighbor, current)
        // if !came_from2.contains(pair) then
        //   frontier2.enqueue(neighbor)
        //   came_from2.addOne(pair)
    // println(reached2.mkString("\n"))
    val set: Set[Vector[String]] = Set.empty[Vector[String]]
    recF("A", Vector())
    def recF(str: String, _path: Vector[String]): Unit =
      println(s"str: $str")
      val path = _path :+ str
      if str == "end" then
        println(path.mkString("|"))
        set.addOne(path)
      else
        val filtered = 
          came_from
          .clone
          .filter(pair => pair._2 == str)
          //  || pair._2 == str)
          // .map(pair => if pair._2 == str then pair else (pair._2, pair._1))
          .filter(pair => 
            !(
              pair._1 == pair._1.toLowerCase &&
              path.contains(pair._1)
            )
          )
    //       //   &&
    //       //   !(
    //       //     if path.contains(pair._1) && path.indexOf(pair._1) > 0 then
    //       //       path(path.indexOf(pair._1) - 1) == pair._2
    //       //     else
    //       //       false
    //       //   )
    //       // )
        println(filtered.mkString(";"))
        // for pair <- filtered do
        //   recF(pair._1, path)
    println(set.mkString("<>"))
    println("wow1")
  
  def day12part2(): Unit =
    println("wow2")

package aoc

object d14:
  import scala.collection.mutable.{Map, Set, Queue}
  
  val start: String = util.readLines("input/14").apply(0).toString
  val polMap: Map[String, String] = Map.empty[String, String]
  util.readLines("input/14")
    .drop(2)
    .map(_.split(" -> "))
    .foreach(str => 
      polMap += (str(0) -> str(1))
    )
    //filter(str => str(0) != 'f').map(_.split(",")).map(p => (p(0).toInt, p(1).toInt))
  // val folds: Array[(Boolean, Int)] =
  //   util.readLines("input/13").filter(str => str(0) == 'f').map(str => str.drop(11)).map(str => str.filter(c => c != '='))
  //   .map(str => 
  //     ( (str.take(1) == "x"), str.drop(1).toInt )
  //   )

  def run(): Unit =
    // part1()
    part2()

  def part1(): Unit =
    var str = start
    // for mapping <- polMap do
    //   println(mapping.toString)
    for iteration <- 1 to 10 do
      var newStr = ""
      for pair <- str.sliding(2) do
        newStr += pair(0) + polMap(pair)
      str = newStr + str.last
      // println(iteration)
    println(s"$str")
    val counts = str.map(c => str.count(c == _))
    val value = counts.max - counts.min
    println(value)

  def part2(): Unit =
    var str = start
    val pairs: Array[String] = polMap.keys.toArray
    var counts: Array[Long] = Array.fill[Long](pairs.length)(0)
    
    // init counts
    for pair <- str.sliding(2) do
      counts(pairs.indexOf(pair)) += 1
    
    // for i <- pairs.indices do
    //   println(s"${pairs(i)}: ${counts(i)}")

    for iteration <- 1 to 40 do
      val newCounts: Array[Long] = Array.fill[Long](pairs.length)(0)
      for i <- pairs.indices do
        val _count = counts(i)
        val s = pairs(i)
        val char = polMap(s)
        val s1 = s(0) + char
        val s2 = char + s(1)
        // println(s"$s, $char, $s1, $s2")
        newCounts(pairs.indexOf(s1)) += _count
        newCounts(pairs.indexOf(s2)) += _count
        // newCounts(i)  -= _count // swap 'indexof' to 'i'
      counts = newCounts
    //   var newStr = ""
    //   for pair <- str.sliding(2) do
    //     newStr += pair(0) + polMap(pair)
    //   str = newStr + str.last
    //   if iteration%10 == 0 then println(iteration)
      // println(iteration)
    // println(s"$str")
    // for i <- pairs.indices do
    //   println(s"${pairs(i)}: ${counts(i)}")
    val s = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    val endcount: Array[Long] = Array.fill[Long](s.length)(0)
    for i <- pairs.indices do
      endcount(s.indexOf(pairs(i)(0))) += counts(i)
      // val s = pairs(i)
      // for c <- s do
      //   endcount(s.indexOf(c)) += counts(i)
    // for cou <- endcount.filter(_ != 0) do
    //   println(cou)
    val value = endcount.max - endcount.filter(v => v != 0).min
    println(value)

import scala.collection.mutable.{Map, Set}
// @main
def day8(): Unit =
  val input: Array[Array[String]] = 
    scala.io.Source.fromFile("input/8").getLines.toArray.map(_.split(" "))
  val screenInput = input.map(xs => xs.take(xs.indexOf("|")))
  val screenOutput = input.map(xs => xs.drop(xs.indexOf("|") + 1))
  day8part1(screenOutput)
  day8part2(screenInput, screenOutput)

def day8part1(input: Array[Array[String]]): Unit =
  val count: Array[Int] = Array.fill[Int](10)(0)
  input.foreach(xs =>
    count(1) += xs.count(_.length == 2)
    count(3) += xs.count(_.length == 3)
    count(4) += xs.count(_.length == 4)
    count(8) += xs.count(_.length == 7)
    )
  println(count.sum)

def key(input: Array[String]): Map[Set[Char], Int] = 
  val setIn: Set[Set[Char]] = Set()
  input.foreach(str => setIn += str.to(Set))
  val l: Array[Set[Char]] = Array.fill[Set[Char]](10)(Set[Char]())
  
  def filterSetIn(str: String = "") = 
    l.foreach(setIn.remove(_))
  
  input.find(_.length == 2).get.foreach(l(1).add(_))
  input.find(_.length == 3).get.foreach(l(7).add(_))
  input.find(_.length == 4).get.foreach(l(4).add(_))
  input.find(_.length == 7).get.foreach(l(8).add(_))
  filterSetIn("st")

  val top: Set[Char] = l(7).diff(l(1)) 
  val tlset: Set[Char] = l(4).diff(l(1))
  val blset: Set[Char] = l(8).diff(l(4) ++ top)
  val rset: Set[Char] = l(1)
  
  filterSetIn("p3")
  setIn.foreach(set => if set.size == 5 && l(1).subsetOf(set) then set.foreach(l(3).add(_)))
  
  val threeset = l(3).diff(l(7))
  var mid = Set[Char]()
  var tl = Set[Char]()
  var bl = Set[Char]()
  var tr = Set[Char]()
  var br = Set[Char]()
  var bottom = Set[Char]()
  for k <- threeset do
    if tlset.contains(k) then 
      mid = Set[Char](k)
      tl = tlset.filter(c => c != k) 
    else 
      bottom = Set[Char](k); 
      bl = blset.filter(c => c != k)

  filterSetIn("p5")
  setIn.foreach(set => if set.size == 5 && tl.subsetOf(set) then set.foreach(l(5).add(_))) 
  
  br = l(5).diff(tl ++ top ++ mid ++ bottom)
  tr = rset.diff(br)
  
  filterSetIn("p0")
  setIn.foreach(set => 
    if set.size == 6 && tr.subsetOf(set) && bl.subsetOf(set) then 
      set.foreach(l(0).add(_))
    )
  
  filterSetIn("p2")
  setIn.foreach(set => if set.size == 5 then set.foreach(l(2).add(_))) //if c=5 then 2 {in the filtered list}
  
  filterSetIn("p6/9")
  setIn.foreach(set => 
    if tr.subsetOf(set) then 
      set.foreach(l(9).add(_))
    else 
      set.foreach(l(6).add(_))
    )
  filterSetIn()
  val map: Map[Set[Char], Int] = Map()
  input.foreach(str => 
    map += (str.to(Set) -> l.indexOf(str.to(Set)))
  )
  map

def day8part2(input: Array[Array[String]], output: Array[Array[String]]): Unit =
  val count: Array[Int] = Array.fill[Int](4)(0)
  for i <- input.indices do
    val map = key(input(i))
    for j <- count.indices do
      count(j) += map(output(i)(j).to(Set))

  val sum = count(0)*1000 + count(1)*100 + count(2)*10 + count(3)*1
  println(sum)
    
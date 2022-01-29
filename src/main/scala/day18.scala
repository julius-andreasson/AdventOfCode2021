// package aoc

// /*
// explode
// split
// Vector[Any]
// */

// object d18:
//   val input: Vector[Any] = 
//     util.readLines("input/18a")
//       .map(str => 
//         str
//         .drop(1)
//         .dropRight(1)
//         .split(",")
//         .map(_.toString.toInt)
//         .toVector
//       ).toVector //.foreach(c => println(c.mkString(";")))
//   printObj(input)
  
//   def printObj(obj: Vector[Any]): Unit =
//     println(objToString(obj))  

//   def objToString(obj: Any): String =
//     val res = obj match
//       case vec: Vector[Any] => (for v <- vec yield objToString(v)).mkString("[", ",", "]")
//       case a: Int           => a.toString
//       case _                => "fail"
//     res

//   // @main
//   def day18(): Unit =
//     day18part1()
//     day18part2()

//   def split(obj: Vector[Any]): Vector[Any] =
//     ???

//   def day18part1(): Unit =
//     def objToString(obj: Any, level: Int): Vector[Any] =
//       val res = obj match
//         case vec: Vector[Any] => 
//           (for v <- vec yield objToString(v, level + 1)).toVector
//         case a: Int           => 
//           if a > 10 then Vector(math.floor(a.toDouble / 2d), math.ceil(a.toDouble / 2d))
//         case _                => 
//           Vector("fail")
//       res
    
//     println(s"wow1")

//   def day18part2(): Unit =
//     println(s"wow2")

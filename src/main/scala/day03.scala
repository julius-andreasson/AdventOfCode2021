package aoc

object d03:
  def run(): Unit =
    val input = util.readLines("input/3")
    part1(input.map(str => str.map(_ == '1').toArray))
    part2(input)

  def binaryToDecimal(input: Array[Boolean]): Int =
    var res = 0
    for i <- input.indices do
      val revIn = input.reverse
      if revIn(i) then
        res += 1 << i
    res


  def part1(input: Array[Array[Boolean]]): Unit =
    val ones = Array.fill[Int](12)(0)
    input.foreach(str =>
      for (i <- 0 until 12) do
        if str(i) then ones(i) += 1
    )
    val epsilon = ones.map(_ <= input.length / 2)
    // Since 'epsilon' is just inverted gamma, there is no need to define
    // it as its own variable. Just invert gamma using '.map(!_)'. 
    val output = (binaryToDecimal(epsilon) * binaryToDecimal(epsilon.map(!_))).toString
    println(s"Part 1: $output")

  def part2(arr: Array[String]): Unit = 
    val output = (binaryToDecimal(ratingCO2(arr)) * binaryToDecimal(ratingOxygen(arr))).toString
    println(s"Part 2: $output")

  def ratingOxygen(arr: Array[String]): Array[Boolean] = 
    var zeroes = Array.fill[Int](12)(0)
    var ones = Array.fill[Int](12)(0)
    var filteredArr = arr.clone
    var found = false
    var x = 0
    while !found do
      for (y <- filteredArr.indices) do
        if filteredArr(y)(x) == '0' then
          zeroes(x) += 1
        else
          ones(x) += 1
      // If more zeroes than ones, keep only zeroes
      if zeroes(x) > ones(x) then
        filteredArr = filteredArr.filter(str => str(x) == '0')
      else
        filteredArr = filteredArr.filter(str => str(x) == '1')
      x += 1
      if filteredArr.length <= 1 then
        found = true
    end while
    filteredArr(0).map(c => c == '1').toArray
  

  def ratingCO2(arr: Array[String]): Array[Boolean] =
    var zeroes = Array.fill[Int](12)(0)
    var ones = Array.fill[Int](12)(0)
    var newArr = arr.clone
    var found = false
    var x = 0
    while !found do
      for (y <- newArr.indices) do
        if newArr(y)(x) == '0' then
          zeroes(x) += 1
        else
          ones(x) += 1
      // if ones >= zeroes, keep only zeroes
      if ones(x) >= zeroes(x) then
        newArr = newArr.filter(str => str(x) == '0')
      else
        newArr = newArr.filter(str => str(x) == '1')
      x += 1
      if newArr.length <= 1 then
        found = true
    end while
    newArr(0).map(c => c == '1').toArray
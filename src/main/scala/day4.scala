import scala.io.Source

// @main
def day4(): Unit =
  val input: Array[String] = Source.fromFile("input/4").getLines.toArray
  val numbers: Array[Int] = input(0).split(",").map(_.toInt)
  var boards: Vector[Array[Array[Int]]] = Vector[Array[Array[Int]]]()
  var currentBoard: Vector[Array[Int]] = Vector[Array[Int]]()
  for i <- 2 until input.length do
    if input(i) == "" then
      boards = boards :+ currentBoard.toArray
      currentBoard = Vector[Array[Int]]()
    else
      currentBoard = currentBoard :+ input(i).split(" ").filter(_ != "").map(_.toInt)
  d4p2(boards, numbers)
  
def d4p2(bingos: Vector[Array[Array[Int]]], numbers: Array[Int]): Unit =
  val loserIndex = getLosingBoardIndex(bingos, numbers) // cycle through all bingos and find first winner
  val loserSum = getSum(bingos(loserIndex._1), numbers)
  printBoard(bingos(loserIndex._1))
  println(s"Number: ${numbers(loserIndex._2)}")
  println(s"WinningBoardIndex: ${loserIndex._1}")
  println(loserSum)
  println(loserSum * numbers(loserIndex._2))

def d4p1(bingos: Vector[Array[Array[Int]]], numbers: Array[Int]): Unit =
  val winnerIndex = getWinningBoardIndex(bingos, numbers) // cycle through all bingos and find first winner
  val winnerSum = getSum(bingos(winnerIndex._1), numbers)
  printBoard(bingos(winnerIndex._1))
  println(s"Number: ${numbers(winnerIndex._2)}")
  println(s"WinningBoardIndex: ${winnerIndex._1}")
  println(winnerSum)
  println(winnerSum * numbers(winnerIndex._2))

// 
def getWinningBoardIndex(bingos: Vector[Array[Array[Int]]], numbers: Array[Int]): (Int, Int) =
  val bingowins: Array[Int] = Array.fill[Int](bingos.length)(-1)
  for i <- bingowins.indices do
    bingowins(i) = getWinningNumberIndex(bingos(i), numbers)
  (bingowins.indexOf(bingowins.min), bingowins.min)

def getLosingBoardIndex(bingos: Vector[Array[Array[Int]]], numbers: Array[Int]): (Int, Int) =
  val bingowins: Array[Int] = Array.fill[Int](bingos.length)(-1)
  for i <- bingowins.indices do
    bingowins(i) = getWinningNumberIndex(bingos(i), numbers)
  (bingowins.indexOf(bingowins.max), bingowins.max)

def deepClone(input: Array[Array[Int]]): Array[Array[Int]] = (for i <- input.indices yield input(i).clone).toArray

def printBoard(board: Array[Array[Int]]): Unit =
  for i <- board.indices do
    println(board(i).mkString(" "))
  println()

def getWinningNumberIndex(bingo: Array[Array[Int]], numbers: Array[Int]): Int =
  val virtualBoard: Array[Array[Int]] = deepClone(bingo)
  var finalIndex = -1
  var n_index = 0
  var found = false
  while !found && n_index < numbers.length do
    val n = numbers(n_index)
    for i <- virtualBoard.indices do
      for j <- virtualBoard(i).indices do
        if virtualBoard(i)(j) == n then
          virtualBoard(i)(j) = 0
          if checkWin(virtualBoard) then
            found = true
    n_index += 1
  n_index - 1

def checkWin(board: Array[Array[Int]]): Boolean =
  var y = 0
  var found = false
  while (!found && y < board.length) do
    found = board(y).forall(_ == 0)  
    y += 1
  end while
  var x = 0
  while (!found && x < board(0).length) do
    var nonZeroFound = false
    y = 0
    while (!nonZeroFound && y < board.length) do
      nonZeroFound = board(y)(x) != 0
      y += 1
    end while
    found = !nonZeroFound
    x += 1
  end while
  found

def getSum(bingo: Array[Array[Int]], numbers: Array[Int]): Int =
  val virtualBoard: Array[Array[Int]] = deepClone(bingo)
  var finalIndex = -1
  var n_index = 0
  var found = false
  while !found && n_index < numbers.length do
    val n = numbers(n_index)
    for i <- virtualBoard.indices do
      for j <- virtualBoard(i).indices do
        if virtualBoard(i)(j) == n then
          virtualBoard(i)(j) = 0
          if checkWin(virtualBoard) then
            found = true
    n_index += 1
  boardSum(virtualBoard)

def boardSum(board: Array[Array[Int]]): Int =
  var sum = 0
  for i <- board.indices do
    sum += board(i).sum
  sum

def d4p2_1(arr: Array[String]): String =
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
    if ones(x) >= zeroes(x) then
      newArr = newArr.filter(str => str(x) == '0')
    else
      newArr = newArr.filter(str => str(x) == '1')
    x += 1
    if newArr.length <= 1 then
      found = true
  end while
  println(newArr.mkString("; "))
  newArr(0)

def d4p2_2(arr: Array[String]): String = 
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
    if zeroes(x) > ones(x) then
      newArr = newArr.filter(str => str(x) == '0')
    else
      newArr = newArr.filter(str => str(x) == '1')
    x += 1
    if newArr.length <= 1 then
      found = true
  end while
  println(newArr.mkString("; "))
  newArr(0)

package aoc

object d04:
  def run(): Unit =
    val input: Array[String] = util.readLines("input/4")
    val numbers: Array[Int] = input(0).split(",").map(_.toInt)
    var boards: Vector[Array[Array[Int]]] = Vector[Array[Array[Int]]]()
    var currentBoard: Vector[Array[Int]] = Vector[Array[Int]]()
    for i <- 2 until input.length do
      if input(i) == "" then
        boards = boards :+ currentBoard.toArray
        currentBoard = Vector[Array[Int]]()
      else
        currentBoard = currentBoard :+ input(i).split(" ").filter(_ != "").map(_.toInt)
    part2(boards, numbers)
    
  def part1(bingos: Vector[Array[Array[Int]]], numbers: Array[Int]): Unit =
    val winnerIndex = getWinningBoardIndex(bingos, numbers) // cycle through all bingos and find first winner
    val winnerSum = getSum(bingos(winnerIndex._1), numbers)
    printBoard(bingos(winnerIndex._1))
    println(s"Number: ${numbers(winnerIndex._2)}")
    println(s"WinningBoardIndex: ${winnerIndex._1}")
    println(winnerSum)
    println(winnerSum * numbers(winnerIndex._2))

  def part2(bingos: Vector[Array[Array[Int]]], numbers: Array[Int]): Unit =
    val loserIndex = getLosingBoardIndex(bingos, numbers) // cycle through all bingos and find first loser
    val loserSum = getSum(bingos(loserIndex._1), numbers)
    printBoard(bingos(loserIndex._1))
    println(s"Number: ${numbers(loserIndex._2)}")
    println(s"LosingBoardIndex: ${loserIndex._1}")
    println(loserSum)
    println(loserSum * numbers(loserIndex._2))
  
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

  def printBoard(board: Array[Array[Int]]): Unit =
    for i <- board.indices do
      println(board(i).mkString(" "))
    println()

  def getWinningNumberIndex(bingo: Array[Array[Int]], numbers: Array[Int]): Int =
    val virtualBoard: Array[Array[Int]] = util.deepClone(bingo)
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
    val virtualBoard: Array[Array[Int]] = util.deepClone(bingo)
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

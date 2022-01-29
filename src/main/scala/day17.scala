package aoc

object d17:
  /*
    Part 1: 
    Constrain the problem by finding extreme values
    Then test ALL combinations within that range
    Return the one with highest y.

    Part 2:
    
  */
  // @main
  def day17(): Unit =
    //target area: x=150..171, y=-129..-70
    val minX = 150
    val maxX = 171
    val minY = -129
    val maxY = -70
    val ymovrange = (minY to 2000)
    val xmovrange = (0 to maxX)

    var nbrOfWays: Int = 0

    var maxiY = 0
    for _dy <- ymovrange; _dx <- xmovrange do
      var localMaxY = 0
      var x = 0; var dx = _dx
      var y = 0; var dy = _dy
      var stop = false
      while !stop do
        // The probe's x position increases by dx.
        x += dx
        // The probe's y position increases by dy.
        y += dy
        // Due to drag, dx changes by 1 toward the value 0. 
        if dx > 0 then dx -= 1
        // Due to gravity, the probe's y velocity decreases by 1.
        dy += -1

        if y > localMaxY then localMaxY = y

        // If it has passed the target area, stop.
        if y < minY || x > maxX then 
          stop = true
        else // If it hasn't, check if it has entered the target area. 
          if x >= minX && y <= maxY then
            nbrOfWays += 1
            if localMaxY > maxiY then maxiY = localMaxY
            stop = true
      end while

    println(s"Max Y: $maxiY")
    println(s"Number of solutions: ${nbrOfWays}")

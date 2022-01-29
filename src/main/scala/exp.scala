object exp {
  def checkNative(i: Int) =
    (i >> 31) | (-i >>> 31)
  
  @main
  def test: Unit =
    // val start = System.nanoTime
    // def elapsed = System.nanoTime - start
    println(Vector.fill(10)(Vector.fill(4)(scala.util.Random().nextInt(256)).mkString(".")).mkString("\n"))
}

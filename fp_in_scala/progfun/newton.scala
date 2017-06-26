
object Newton {
  def abs(x: Double): Double = if (x < 0) -x else x

  def sqrtIter(guess: Double, x: Double): Double =
    if (isGoodEnough(guess, x)) guess else {
      sqrtIter(improve(guess, x), x)
    }

  def isGoodEnough(guess: Double, x: Double): Boolean = {
    val error = abs(guess*guess - x)
    (error < 0.0001) && (error / x < 0.0001)
  }

  def improve(guess: Double, x: Double): Double = (guess + x / guess) / 2

  def sqrt(x: Double): Double = sqrtIter(1.0, x)

  def main(args: Array[String]): Unit  = println(sqrt(1e50))
}

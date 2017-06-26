object Factorial {
  def factorial(n: Int): Int = {
    def tailRecFactorial(result: Int, n: Int): Int = n match {
      case 0 => result
      case x => tailRecFactorial(result*n, n-1)
    }

    tailRecFactorial(1, n)
  }

  def main(args: Array[String]) = println(factorial(10))
}

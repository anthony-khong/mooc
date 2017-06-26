object RecSum extends App {
  def sum(f: Int => Int)(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, acc + f(a))
    }
    loop(a, 0)
  }

  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, init: Int)(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, combine(acc, f(a)))
    }
    loop(a, init)
  }
}

package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    var i, count = 0
    while (i < chars.size) {
      if (chars(i) == '(')
        count += 1
      else if (chars(i) == ')')
        count -= 1
      if (count < 0)
        return false
      i += 1
    }
    count == 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    //def traverse(from: Int, until: Int, arg1: Int, arg2: Int): (Int, Int) = {
    def traverse(from: Int, until: Int): (Int, Int) = {
      var left = from
      var right = until
      var count, min = 0
      while (left < right) {
        if (chars(left) == '(')
          count += 1
        else if (chars(left) == ')')
          count -= 1
        if (count < min)
          min = count
        left += 1
      }
      (count, min)
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold) {
        traverse(from, until)
      } else {
        val mid = (from + until) / 2
        val ((endLeft, minLeft), (endRight, minRight)) = parallel(reduce(from, mid), reduce(mid, until))
        val realMinRight = minRight + endLeft
        (endLeft + endRight, if (realMinRight < minLeft) realMinRight else minLeft)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}

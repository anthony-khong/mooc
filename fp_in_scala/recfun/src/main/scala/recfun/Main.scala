package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    val balanceCases = List(
      "(if (zero? x) max (/ 1 x))",
      "I told him (that it’s not (yet) done). (But he wasn’t listening)",
      ":-)",
      "())("
      )
    for (testCase <- balanceCases) {
      println("Testing balance of ".concat(testCase))
      println(balance(testCase.toList))
    }

    val changeCases = List(
      (4, List(1, 2))
    )
    for ((money, coins) <- changeCases) {
      println("Num ways to change ".concat(money.toString))
      println("with coins ".concat(coins.toString))
      val answer = countChange(money, coins)
      println("Answer: ".concat(answer.toString))
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = (c, r) match {
      case (0, _) => 1
      case (x, y) if (x > y) => 0
      case (x, y) => pascal(x-1, y-1) + pascal(x, y-1)
    }

  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def recBalance(unclosed: Int, chars: List[Char]): Boolean = chars match {
        case Nil => unclosed == 0
        case x :: xTail => {
          val newScore = unclosed + bracketScore(x)
          if (newScore < 0) false else recBalance(newScore, xTail)
        }
      }

      def bracketScore(x: Char): Int = x match {
        case '(' => 1
        case ')' => -1
        case _ => 0
      }

      recBalance(0, chars)
    }


  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = (money, coins) match {
      case (_, Nil) => 0
      case (0, _) => 1
      case (m, _) if (m < 0) => 0
      case (m, coin :: restOfCoins) => {
        val countCoinUsed = countChange(m - coin, coin :: restOfCoins)
        val countCoinNotUsed = countChange(m, restOfCoins)
        countCoinUsed + countCoinNotUsed
      }
    }

  }

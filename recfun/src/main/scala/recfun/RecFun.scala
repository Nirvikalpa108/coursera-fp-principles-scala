package recfun

import com.sun.xml.internal.bind.v2.TODO

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = if (c == 0 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)

  // IF c = 0, then the value is 1
  // IF c == r, the value is 1
  // get the left co-ord above our argument co-ord ( left c = c - 1, left row = r - 1)
  // get the right co-ord above our argument ( right c = c, right row = r - 1)
  // now I just need to build this recursive function, so the function itself can work from the top of the triangle to find the argument's value
  // TOLEARN: guard conditions
  // file:///Users/amina_adewusi/Downloads/Screen%20Shot%202020-04-30%20at%2010.50.59.png

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceGivenTheNumberOfOpenBrackets(numberOfOpenBracketsSoFar: Int, theRestOfTheString: List[Char]): Boolean = {
      // I give you the number of open brackets, tell me if the rest of this string is balanced.
      if(theRestOfTheString.isEmpty) {
        numberOfOpenBracketsSoFar == 0
      } else if(numberOfOpenBracketsSoFar < 0 ) false else {
        val changeTheNumberOfOpenBrackets: Int = theRestOfTheString.head match {
          case '(' => 1
          case ')' => -1
          case _ => 0
        }
        val newNumberOfOpenBracketsSoFar = numberOfOpenBracketsSoFar + changeTheNumberOfOpenBrackets
        balanceGivenTheNumberOfOpenBrackets(newNumberOfOpenBracketsSoFar, theRestOfTheString.tail)
      }
    }
    balanceGivenTheNumberOfOpenBrackets(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1 else if (money < 0) 0 else if (coins.isEmpty) 0 else {
      val numberOfWaysUsingHeadCoin: Int = countChange(money - coins.head, coins)
      val numberOfWaysNotUsingHeadCoin: Int = countChange(money, coins.tail)
      numberOfWaysUsingHeadCoin + numberOfWaysNotUsingHeadCoin
    }
  }
}

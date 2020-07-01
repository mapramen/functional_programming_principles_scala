package recfun

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
  def pascal(c: Int, r: Int): Int = 
    if(r < 0) 0
    else if(r < c) 0
    else if(c == 0) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def charToInt(c: Char): Int = c match {
      case '(' => 1
      case ')' => -1
      case _ => 0
    }

    def checkValidParenthesisHelper(acc: Int, s: List[Char]): Boolean = {
      if(acc < 0) false
      else if(s == Nil) acc == 0
      else checkValidParenthesisHelper(acc + charToInt(s.head), s.tail)
    }

    checkValidParenthesisHelper(0, chars);
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if(money < 0) 0
    else if(money == 0) 1
    else if(coins == Nil) 0
    else countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }
}

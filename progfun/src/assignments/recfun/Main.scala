package recfun

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
      @tailrec
      def step(chars: List[Char], acc: Int): Boolean = {
      if (chars.isEmpty) return (acc == 0)
      if (acc < 0) return false

      val head = chars.head
      var accModifier: Int = 0
      if (head == '(') accModifier = 1
      else if (head == ')') accModifier = - 1
      
      step(chars.tail, acc + accModifier)
    }

    return step(chars, 0)
  }               

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def step(moneyLeft: Int, coinsLeft: List[Int]): Int = {
    	if (moneyLeft == 0) return 1
      if (moneyLeft < 0) return 0
      if (coinsLeft.isEmpty) return 0
      
      val highestDenom = coinsLeft.head
      val changeLeft = moneyLeft - highestDenom
      return step(changeLeft, coinsLeft) + step(moneyLeft, coinsLeft.tail)
    }
    
    return step(money, coins.sorted.distinct)
  }                  
}

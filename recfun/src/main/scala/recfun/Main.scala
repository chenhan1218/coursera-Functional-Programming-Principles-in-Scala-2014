package recfun
import common._

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
  def pascal(c: Int, r: Int): Int = {
    if (c==0) 1
    else r * pascal(c-1, r-1) /c
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceCount(chars: List[Char], left: Int): Boolean = {
      if(chars.isEmpty) left==0
      else if (chars.head=='(') balanceCount(chars.tail, left+1)
      else if (chars.head==')')
        if (left<=0) false
        else balanceCount(chars.tail, left-1)
      else balanceCount(chars.tail, left)
    }
    
    balanceCount(chars,0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if( money == 0 ) 1
    else if( coins.isEmpty ) 0
    else {
      if( money >= coins.head ) countChange(money, coins.tail) + countChange(money-coins.head, coins)
      else countChange(money, coins.tail)
    }
  }
}

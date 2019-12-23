import scala.annotation.tailrec

object ChapterII {

  /**
   * Fibonacci serie
   * @param n position in the serie
   * @return value of the number at this position
   */
  def fib(n: Int): Int = {
    var cpt = 3
    @tailrec
    def loop(l: Int, m: Int): Int = {
      if (cpt < n) {
        cpt += 1
        loop(m, l + m)
      } else l + m
    }
    loop(0, 1)
  }

  /**
   * Comparison type Integer
   * @return Boolean state
   */
  def ordered: (Int, Int) => Boolean = (a: Int, b: Int) => if (a > b) false else true

  /**
   * Is sorted implicit function
   * @param as Array of A
   * @param ordered Comparison function
   * @tparam A Implicit type
   * @return Boolean state is sorted or not
   */
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    for(i <- 0 to as.size -2) {
      if (ordered(as(i), as(i+1)) == false) {
        return false
      }
    }
    true
  }

  /**
   * Currying example
   * @param f Implicit function
   * @tparam A
   * @tparam B
   * @tparam C
   * @return Sign.
   */
  def curry[A,B,C](f: (A,B) => C): A => (B => C) = {
    a => b => f(a,b)
  }

  /**
   * Uncurrying example
   * @param f Implicit function
   * @tparam A
   * @tparam B
   * @tparam C
   * @return Sign.
   */
  def uncurry[A,B,C](f: A => B => C): (A,B) => C = {
    (a, b) => f(a)(b)
  }

  /**
   * Compose example
   * @param f Implicit function
   * @param g Implicit function
   * @tparam A
   * @tparam B
   * @tparam C
   * @return Sign.
   */
  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    b => f(b)
  }

  def main(args: Array[String]): Unit = {
    println("Fibonacci:")
    println(fib(6))
    println("isSorted Integers:")
    println(isSorted(Array(0, 1, 2), ordered))
  }

}

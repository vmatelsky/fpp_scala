package week2

object exersise {

  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  def factorial(n: Int): Int = {
    def loop(acc: Int, n: Int): Int = if (n == 0) acc else loop(n * acc, n - 1)

    loop(1, n)
  }                                               //> factorial: (n: Int)Int
  factorial(2)                                    //> res0: Int = 2
  factorial(4)                                    //> res1: Int = 24

  def sum(f: Int => Int, a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int =
      if (a > b) acc
      else loop(a + 1, acc + f(a))

    loop(a, 0)
  }                                               //> sum: (f: Int => Int, a: Int, b: Int)Int

  sum(x => x * x, 3, 5)                           //> res2: Int = 50

}
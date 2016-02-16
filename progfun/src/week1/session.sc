package week1

object session {
  println("Welcome to the Scala worksheet")
  1 + 2

  def abs(x: Double) = if (x > 0) x else -x
  def sqrt(x: Double): Double = {

    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))

    def isGoodEnough(guess: Double) =
      abs(x / guess - guess) < 0.000001

    def improve(guess: Double): Double =
      (guess + x / guess) / 2

    sqrtIter(1.0)
  }
 

  sqrt(4)
  sqrt(1e-6)

  sqrt(0.001)
  sqrt(0.1e-20)
  sqrt(1.0e20)
  sqrt(1.0e50)

  def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  gcd(5, 10)
  gcd(3, 7)
  gcd(14, 21)

  def factorial(n: Int): Int =
    if (n == 0) 1 else n * factorial(n - 1)
    
  factorial(2)
  factorial(4)

}
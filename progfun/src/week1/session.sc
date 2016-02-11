package week1

object session {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  1 + 2                                           //> res0: Int(3) = 3

  def abs(x: Double) = if (x > 0) x else -x       //> abs: (x: Double)Double
  def sqrt(x: Double): Double = {

    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))

    def isGoodEnough(guess: Double) =
      abs(x / guess - guess) < 0.000001

    def improve(guess: Double): Double =
      (guess + x / guess) / 2

    sqrtIter(1.0)
  }                                               //> sqrt: (x: Double)Double

  sqrt(4)                                         //> res1: Double = 2.0000000929222947
  sqrt(1e-6)                                      //> res2: Double = 0.0010000001533016628

  sqrt(0.001)                                     //> res3: Double = 0.03162278245070105
  sqrt(0.1e-20)                                   //> res4: Double = 9.536743167557754E-7
  sqrt(1.0e20)                                    //> res5: Double = 1.0E10
  sqrt(1.0e50)                                    //> res6: Double = 1.0E25

  def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
                                                  //> gcd: (a: Int, b: Int)Int

  gcd(5, 10)                                      //> res7: Int = 5
  gcd(3, 7)                                       //> res8: Int = 1
  gcd(14, 21)                                     //> res9: Int = 7

  def factorial(n: Int): Int =
    if (n == 0) 1 else n * factorial(n - 1)       //> factorial: (n: Int)Int
    
  factorial(2)                                    //> res10: Int = 2
  factorial(4)                                    //> res11: Int = 24

}
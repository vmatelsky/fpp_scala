package week1

object session {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  1 + 2                                           //> res0: Int(3) = 3

  def abs(x: Double) = if (x > 0) x else -x       //> abs: (x: Double)Double

  def sqrtIter(guess: Double, x: Double): Double =
    if (isGoodEnough(guess, x)) guess
    else sqrtIter(improve(guess, x), x)           //> sqrtIter: (guess: Double, x: Double)Double

  def isGoodEnough(guess: Double, x: Double) =
    abs(x / guess - guess) < 0.000001             //> isGoodEnough: (guess: Double, x: Double)Boolean

  def improve(guess: Double, x: Double): Double =
    (guess + x / guess) / 2                       //> improve: (guess: Double, x: Double)Double

  def sqrt(x: Double): Double = sqrtIter(1.0, x)  //> sqrt: (x: Double)Double

  sqrt(4)                                         //> res1: Double = 2.0000000929222947
  sqrt(1e-6)                                      //> res2: Double = 0.0010000001533016628

  sqrt(0.001)                                     //> res3: Double = 0.03162278245070105
  sqrt(0.1e-20)                                   //> res4: Double = 9.536743167557754E-7
  sqrt(1.0e20)                                    //> res5: Double = 1.0E10
  sqrt(1.0e50)                                    //> res6: Double = 1.0E25
 

}
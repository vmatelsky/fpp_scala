package week4

object week4 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  val t: List[Int] = new Cons(1, new Nil)         //> t  : week4.List[Int] = week4.Cons@62059579
	List.apply(1, 2)                          //> res0: week4.List[Int] = week4.Cons@1d6e5ecb

  def nth[T](n: Int, xs: List[T]): T = {
    if (xs.isEmpty) throw new IndexOutOfBoundsException()
    if (n == 0) xs.head
    else nth(n - 1, xs.tail)
  }                                               //> nth: [T](n: Int, xs: week4.List[T])T

  new Function1[Int, Int] {
    def apply(x: Int) = x * x
  }                                               //> res1: Int => Int = <function1>

}
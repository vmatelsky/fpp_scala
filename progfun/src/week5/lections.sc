package week5

import math.Ordering

object lections {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  def init[T](xs: List[T]): List[T] = xs match {
    case List()  => throw new Error("is empty")
    case List(x) => List()
    case y :: ys => y :: init(ys)
  }                                               //> init: [T](xs: List[T])List[T]

  def removeAt[T](n: Int, xs: List[T]): List[T] = (xs take n) ::: (xs drop n + 1)
                                                  //> removeAt: [T](n: Int, xs: List[T])List[T]

  def msort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {

      def merge(xs: List[T], ys: List[T]): List[T] =
        (xs, ys) match {
          case (Nil, ys) => ys
          case (xs, Nil) => xs
          case (x :: xs1, y :: ys1) =>
            if (ord.lt(x, y)) x :: merge(xs1, ys)
            else y :: merge(xs, ys1)
        }

      val (fst, snd) = xs splitAt n
      merge(msort(fst), msort(snd))
    }
  }                                               //> msort: [T](xs: List[T])(implicit ord: scala.math.Ordering[T])List[T]

  val num = List(3, 5, 6, -1, 10)                 //> num  : List[Int] = List(3, 5, 6, -1, 10)
  msort(num)                                      //> res0: List[Int] = List(-1, 3, 5, 6, 10)

  class Animal(val name: String) {

    override def toString = name
  }

  trait AnimalOrdering extends Ordering[Animal] {
    def compare(x: Animal, y: Animal) =
      x.name.compare(y.name)
  }
  implicit object AnimalOrd extends AnimalOrdering

  val animals = List(new Animal("dog"), new Animal("cat"))
                                                  //> animals  : List[week5.lections.Animal] = List(dog, cat)
  msort(animals)                                  //> res1: List[week5.lections.Animal] = List(cat, dog)

  def squareList1(xs: List[Int]): List[Int] = xs match {
    case Nil     => Nil
    case y :: ys => (y * y) :: squareList1(ys)
  }                                               //> squareList1: (xs: List[Int])List[Int]

  def squareList2(xs: List[Int]): List[Int] =
    xs map (x => x * x)                           //> squareList2: (xs: List[Int])List[Int]

  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case x :: xs1 => {
      val (first, rest) = xs span (y => y == x)
      first :: pack(rest)
    }
  }                                               //> pack: [T](xs: List[T])List[List[T]]

  val data = List("a", "a", "a", "b", "c", "c", "a")
                                                  //> data  : List[String] = List(a, a, a, b, c, c, a)
  pack(data)                                      //> res2: List[List[String]] = List(List(a, a, a), List(b), List(c, c), List(a)
                                                  //| )

  def encode[T](xs: List[T]): List[(T, Int)] = pack(xs) map (ys => (ys.head, ys.length))
                                                  //> encode: [T](xs: List[T])List[(T, Int)]
	encode(List("a", "a", "a", "b", "c", "c", "a"))
                                                  //> res3: List[(String, Int)] = List((a,3), (b,1), (c,2), (a,1))
}
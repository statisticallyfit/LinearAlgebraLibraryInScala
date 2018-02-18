package linalg.kernel

import linalg.implicits._
/**
  *
  */
//TODO try to implement Imaginary as Number type and see if you still need NumberConversion operations
case class Imaginary[R: RealNumber](im: R) {

     //implicit def i: Imaginary[R] = this

     override def toString: String = Show[Imaginary[R]].show(this)
}

object Imaginary {

     def ZERO[R](implicit gen: RealNumber[R]): Imaginary[R] = new Imaginary(gen.zero)
     def ONE[R](implicit gen: RealNumber[R]): Imaginary[R] = new Imaginary(gen.one)
     def TWO[R](implicit gen: RealNumber[R]): Imaginary[R] = new Imaginary(gen.two)
}

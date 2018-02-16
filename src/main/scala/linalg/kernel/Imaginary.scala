package linalg.kernel

import linalg.implicits._
/**
  *
  */
//TODO try to implement Imaginary as Number type and see if you still need NumberConversion operations
case class Imaginary[R: linalg.RealNumber](im: R) {

     implicit def i: Imaginary[R] = this

     override def toString: String = im match {
          case _: Rational => im.isNegative match {
               case true => " - (" + im.negate().toString + ")" + "i"
               case false => " + (" + im.toString + ")" + "i"
          }
          case _ => im.isNegative match {
               case true => " - " + im.negate().toString + "i"
               case false => " + " + im.toString + "i"
          }
     }
}

object Imaginary {

     def ZERO[R](implicit gen: linalg.RealNumber[R]): Imaginary[R] = new Imaginary(gen.zero)
     def ONE[R](implicit gen: linalg.RealNumber[R]): Imaginary[R] = new Imaginary(gen.one)
     def TWO[R](implicit gen: linalg.RealNumber[R]): Imaginary[R] = new Imaginary(gen.two)
}

package linalg.vector

import linalg.implicits._
import linalg._
//
///**
//  *
//  */
//
////note: can make it parametrized by Number if you like (in case you want Complex type too)
//
////todo either make it extend vector or make it something that implements veclike
class Polynomial[R: RealNumber](private val ps: R*) /*extends Vector[R](ps:_*)*/ {

     override def toString: String = Show[Polynomial[R]].show(this)
     //this.asInstanceOf[Polynomial[R]].show
}
//
//
object Polynomial {

     def apply[R: RealNumber](ps: R*): Polynomial[R] = new Polynomial[R](ps:_*)

     def ONE[R](len: Int)(implicit gen: RealNumber[R]): Polynomial[R] =
          new Polynomial(List.fill[R](len)(gen.one):_*)

     def ZERO[R](len: Int)(implicit gen: RealNumber[R]): Polynomial[R] =
          new Polynomial(List.fill[R](len)(gen.zero):_*)

     // ------------

     /*implicit def PolynomialIsRing[R: RealNumber] = new Ring[Polynomial[R]]{

          val gen = implicitly[RealNumber[R]]
          val zero: Polynomial[R] = Polynomial(gen.zero) // just simple constant 0

          def plus(p: Polynomial[R], q: Polynomial[R]): Polynomial[R] = ??? //todo p + q
          def negate(p: Polynomial[R]): Polynomial[R] = ??? //todo p.negate()
          def times(p: Polynomial[R], q: Polynomial[R]): Polynomial[R] = ??? //todo - scale iteratively by each const q
     }*/
}
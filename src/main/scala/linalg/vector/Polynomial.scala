package linalg.vector

import linalg.theory._
import linalg.numeric._
import linalg.syntax.ShowSyntax._
import linalg.show.Show._

/**
  *
  */

//note: can make it parametrized by Number if you like (in case you want Complex type too)

class Polynomial[R: RealLike](val ps: R*) extends Vector[R](ps:_*) {

     override def toString: String = Polynomial(ps:_*).show
}


object Polynomial {

     def apply[R: RealLike](ps: R*): Polynomial[R] = new Polynomial[R](ps:_*)

     def ONE[R](len: Int)(implicit gen: RealLike[R]): Polynomial[R] =
          new Polynomial(List.fill[R](len)(gen.one):_*)

     def ZERO[R](len: Int)(implicit gen: RealLike[R]): Polynomial[R] =
          new Polynomial(List.fill[R](len)(gen.zero):_*)

     // ------------

     implicit def PolynomialIsRing[R: RealLike] = new Ring[Polynomial[R]]{

          import linalg.syntax.VectorLikeSyntax._
          import Vector._

          val gen = implicitly[RealLike[R]]
          val zero: Polynomial[R] = Polynomial(gen.zero) // just simple constant 0

          def plus(p: Polynomial[R], q: Polynomial[R]): Polynomial[R] = ??? //todo p + q
          def negate(p: Polynomial[R]): Polynomial[R] = ??? //todo p.negate()
          def times(p: Polynomial[R], q: Polynomial[R]): Polynomial[R] = ??? //todo - scale iteratively by each const q
     }
}
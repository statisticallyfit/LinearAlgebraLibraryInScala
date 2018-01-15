package linalg.theory

import linalg.numeric._
import linalg.numeric.Number
import linalg.numeric.Number._
import linalg.theory._



import cats.instances.int._
import cats.instances.tuple._
import cats.kernel.Eq
import cats.laws.discipline._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{FunSuite, Matchers}
import org.typelevel.discipline.scalatest.Discipline



/**
  * Concrete class examples are here.
  */
case class Polynomial[N: Number](coefs: N*)

// ---------------------------------------------------------------------------------------
/**
  * Concrete implementation of Space  typeclasses are here.
  */
object VectorSpace {

}

object InnerProductSpace{
     implicit def PolyIsInner[N: Number] = new InnerProductSpace[Polynomial[N], N] {

          val gen = implicitly[Number[N]]

          val zero: Int => Polynomial[N] = n => Polynomial(List.fill[N](n)(gen.zero):_*)
          val one: Int => Polynomial[N] = n => Polynomial(List.fill[N](n)(gen.one):_*)

          def innerProduct(p: Polynomial[N], q: Polynomial[N]): N ={
               p.coefs.zip(q.coefs).map{case (pc, qc) => pc* qc}.sum
          }

          def scale(p: Polynomial[N], factor: N): Polynomial[N] = Polynomial(p.coefs.map(_ * factor):_*)

          def plus(p: Polynomial[N], q: Polynomial[N]): Polynomial[N] =
               Polynomial(p.coefs.zip(q.coefs).map{case (pc, qc) => pc + qc}:_*)

          def negate(p: Polynomial[N]): Polynomial[N] = Polynomial(p.coefs.map(_.negate()):_*)
     }
}


class PolyInnerProductSpaceSpec extends FunSuite with Matchers with Discipline {

     implicit def arbitraryNumber[N: Number](implicit a: Arbitrary[N]): Arbitrary[N] ={
          val realNumber = for {
               double <- Gen.choose(Double.MinValue + 1, Double.MaxValue)
          } yield Real(double)


          val rationalNumber = for {

               validNumers <- Gen.choose(Integer.MIN_VALUE + 1, Integer.MAX_VALUE)

               validDenoms <- Gen.choose(Integer.MIN_VALUE + 1, Integer.MAX_VALUE)
          }
     }


     implicit def arbitraryPolynomial[N: Number](implicit a: Arbitrary[N]): Arbitrary[Polynomial[N]] ={


          //Arbitrary(Gen.frequency((1, Polynomial(Gen.frequency((10, arbitrary[N].map(n => n)))))))
     }
}
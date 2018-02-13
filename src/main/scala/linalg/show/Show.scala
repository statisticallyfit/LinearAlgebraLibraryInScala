package linalg.show

import linalg.numeric._
import linalg.vector._

/**
  *
  */

trait Show[S] {

     def show(x: S): String
}
object Show {

     implicit object IntHasShow extends Show[Int] {def show(x: Int): String = x.toString}
     implicit object DoubleHasShow extends Show[Double] {def show(x: Double): String = x.toString}
     implicit object RealHasShow extends Show[Real] { def show(x: Real): String = x.double.toString }

     implicit object RationalHasShow extends Show[Rational] {
          def show(x: Rational): String = x.den match {
               case 1 => x.num.toString
               case _ => x.num.toString + "/" + x.den.toString
          }
     }

     implicit def ComplexHasShow[R : RealLike] = new Show[Complex[R]] {
          def show(x: Complex[R]): String = x.re.toString + Imaginary(x.im).toString
     }

     implicit def VectorHasShow[N: Number] = new Show[Vector[N]]{
          def show(v: Vector[N]): String = s"Vector(${v.elements})"
     }

     implicit def PolynomialHasShow[R: RealLike] = new Show[Polynomial[R]]{
          def show(v: Polynomial[R]): String = "nothing here" //todo to implement
     }

     implicit def SetVecHasShow[N: Number] = new Show[SetOfVectors[N]]{
          def show(vset: SetOfVectors[N]): String = "nothing here" //todo to implement
     }
}
package linalg.util

import linalg.numeric._


import org.apache.commons.lang3.math.Fraction
import scala.language.implicitConversions


/*trait Show[S] {
     def show(s: S): String
}

object Show {

     implicit class ShowOps[S](thing: S)(implicit ev: Show[S]){
          def show: String = ev.show(thing)
     }

     implicit def ComplexString[N : Number : Show] = new Show[Complex[N]]{
          def show(s: Complex[N]): String = s.re.show + " + " + s.im.show + "i" //todo fix later
     }

     implicit val RealString = new Show[Real] {
          def show(s: Real): String = s.value.toString
     }

     implicit val RationalString = new Show[Rational] {
          def show(s: Rational): String = s.num + " / " + s.denom //todo fix later
     }

     implicit val NaturalString = new Show[Natural] {
          def show(s: Natural): String = s.value.toString
     }
}*/




// ---------------------------------------------------------------------------------------------------


object ImplicitConversions {

     implicit def intToRational(int: Int): Rational = Rational(int, 1)

     implicit def doubleToRational(double: Double): Rational ={
          val f: Fraction = Fraction.getFraction(double)
          Rational(f.getNumerator, f.getDenominator)
     }
     /*implicit def complexToString[N : Number](complex: Complex[N]): String = complex.show()
     implicit def rationalToString(rational: Rational): String = rational.show()
     implicit def realToString(real: Real): String = real.show()
     implicit def naturalToString(natural: Natural): String = natural.show()*/
}

//package util
//
//
//import number._
//import matrix._
//import vector.VectorSet
//
//import scala.language.implicitConversions
////note: when I add this, the wavy line under implicit keyword below goes away.
//import scala.reflect.runtime.universe._
///**
//  *
//  */
//object Implicits {
//
//     implicit def double2Rational(double: Double): Rational = Rational(double)
//     implicit def double2Real(double: Double): Real = Real(double)
//
//
//     implicit def VectorSet2Matrix[N <: Number[N]: TypeTag](vset: VectorSet[N]): Matrix[N] =
//          new Matrix(vset.getColumns():_*)
//
//     /*implicit def VectorSet2SquareMatrix[N <: Number[N]: TypeTag](vset: VectorSet[N]): SquareMatrix[N] =
//          new SquareMatrix(vset.getColumns():_*)*/
//
//     //implicit def tovecset[N <: Number[N]](mat: Matrix[N]): VectorSet[N] = new VectorSet[N](mat.getColumns():_*)
//
//     //note making vecset to vector for spanning methods
//     /*implicit def Vector2VecSet[N <: Number[N]: TypeTag](vec: Vector[N]): VectorSet[N]
//          = new VectorSet[N](vec:_*)*/
//
//
//     //note: fixing nullspace[nothing] into nullspace[n]
//     /*implicit def MatNothing2MatN[N <: Number[N]: TypeTag](mat: Matrix[Any]): Matrix[N] ={
//          Matrix.fromLists[N](mat.getColumns().map(vec => vec.toList.map(e => e.asInstanceOf[N])):_*)
//     }*/
//}

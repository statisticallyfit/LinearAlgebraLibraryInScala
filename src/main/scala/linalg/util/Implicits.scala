package linalg.util

import linalg.numeric._
import linalg.matrix._
import linalg.vector._



import scala.language.implicitConversions



object Implicits {
     implicit def realToDouble(r: Real): Double = r.value

     implicit def complexToDouble[N: Number](complex: Complex[N]): Double = complex.abs()

     implicit def doubleToGeneralN[N <: Number[N]](double: Double)(implicit n: Number[N]): N = {

          n match {
               case _: Complex[N] =>
               case _: Real => n.one

          }
     }

     //note: all well and good but we won't ever find the combo of realnum + numerical, will we? Needs to be
     // an actual class, and THAT is exactly what we can't get due to type issues.
     /*implicit class RealExt(real: RealNum with Numerical[RealNum]) {
          def +(that: RealNum): RealNum = new RealNum(real.value + that.value)
     }*/
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

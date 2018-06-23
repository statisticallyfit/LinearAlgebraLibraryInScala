package linalg.instances.linear


import spire.algebra.Eq

import linalg.implicits._
import linalg._
import linalg.matrix.Matrix
import linalg.vector.{SetOfVectors, Vector}
import linalg.util._

import scala.language.higherKinds
import scala.language.implicitConversions
import scala.util.control.Breaks.{break, breakable}
import org.apache.commons.lang3.StringUtils

import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.universe._
import scala.util.control.Breaks._




class MatrixThings[N: Number] {

     //TODO GOAL: implement matrixlike things without re-implementing them again, since similar implementations for
     // TODO for setveclike and matrix things.

     class MatrixIsMatrixLike extends MatrixLike[Matrix[N], N]{

          def power(m: Matrix[N], exp: N) = ???

          def inverse(m: Matrix[N]) = ???

          def transpose(m: Matrix[N]) = ???

          def conjugateTranspose(m: Matrix[N]) = ???

          def adjoint(m: Matrix[N]) = ???

          def cofactor(m: Matrix[N]) = ???

          def minor(m: Matrix[N]) = ???

          def minor(m: Matrix[N], rowIndex: Int, colIndex: Int) = ???

          def determinant(m: Matrix[N]) = ???

          def trace(m: Matrix[N]) = ???

          def identity(size: Int) = ???

          def rowReducedEchelon(m: Matrix[N]) = ???

          def rowEchelon(m: Matrix[N]) = ???

          def isZero(v: Matrix[N]) = ???

          val zero = ???
          val one = ???

          def plus(v: Matrix[N], w: Matrix[N]) = ???

          def negate(v: Matrix[N]) = ???

          def scale(v: Matrix[N], constant: N) = ???
     }


     val matrixLike = new MatrixIsMatrixLike
}


trait MatrixInstances {

     implicit final def matrixIsMatrixLike[N: Number] = new MatrixThings[N].matrixLike


}

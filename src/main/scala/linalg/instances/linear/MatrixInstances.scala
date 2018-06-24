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


     class MatrixHasAbsoluteValue extends Absolute[Matrix[N], Matrix[N]] {

          def absoluteValue(mat: Matrix[N]): Matrix[N] =
               Matrix(mat.getColumns().map(vec => vec.abs()):_*)
     }

     class MatrixHasDimension extends Dimension[Matrix[N]]{
          def dimension(mat: Matrix[N]): Int = mat.getColumns().head.dimension()
     }

     class MatrixHasEq extends Eq[Matrix[N]]{
          def eqv(mat1: Matrix[N], mat2: Matrix[N]): Boolean = {
               Util.Gen.ensureSize(mat1, mat2)

               mat1.getColumns()
                    .zip(mat2.getColumns())
                    .forall(colPair => Eq[Vector[N]].eqv(colPair._1, colPair._2))
          }
     }

     class MatrixIsMonoid extends Monoid[Matrix[N]]{

          val zero: Matrix[N] = Matrix(Vector.ZERO[N](1))

          def plus(mat1: Matrix[N], mat2: Matrix[N]): Matrix[N] ={
               Util.Gen.ensureSize(mat1, mat2)
               Matrix(mat1.getColumns().zip(mat2.getColumns())
                    .map(colPair => colPair._1 + colPair._2):_*)
          }
     }

     class MatrixIsAbelianGroup extends MatrixIsMonoid with AbelianGroup[Matrix[N]]{
          def negate(mat: Matrix[N]): Matrix[N] = Matrix(mat.getColumns().map(c => c.negate()):_*)
     }

     class MatrixIsVectorSpace extends MatrixIsAbelianGroup with VectorSpace[Matrix[N], N]{
          val one: Matrix[N] = Matrix(Vector.ONE[N](1))

          def scale(v: Matrix[N], factor: N): Matrix[N] =
               Matrix(v.getColumns().map(col => col.scale(factor)):_*)
     }

     class MatrixIsSetVecLike extends MatrixIsVectorSpace with SetVecLike[Matrix[N], N]{
          def isZero(mat: Matrix[N]): Boolean = mat.getColumns().forall(col => col.isZero)

          def rowEchelon(mat: Matrix[N]): Matrix[N] =
               Matrix(Util.Gen.rowEchelon[N](mat).getColumns():_*)

          def rowReducedEchelon(mat: Matrix[N]): Matrix[N] =
               Matrix(Util.Gen.rowReducedEchelon[N](mat).getColumns():_*)

     }

     class MatrixIsMatrixLike extends MatrixIsSetVecLike with MatrixLike[Matrix[N], N] {

          def power(mat1: Matrix[N], exp: N): Matrix[N] = ???

          def inverse(mat: Matrix[N]): Matrix[N] = ??? // TODO - solve using augmented?

          def transpose(mat: Matrix[N]): Matrix[N] = Matrix(mat.getRows():_*)

          def conjugateTranspose(mat: Matrix[N]): Matrix[N] =
          /*
          def power(m: M, exp: F): M //use linear algebra formula for matrix powers
     def inverse(m: M): M
     def transpose(m: M): M
     def conjugateTranspose(m: M): M
     def adjoint(m: M): M
     def cofactor(m: M): M
     def minor(m: M): M
     def minor(m: M, rowIndex: Int, colIndex: Int): F
     def determinant(m: M): M
     def trace(m: M): F
           */
     }



     //span, basis ... etc


     val absolute = new MatrixHasAbsoluteValue
     val eq = new MatrixHasEq
     val dim = new MatrixHasDimension
     val monoid = new MatrixIsMonoid
     val abelian = new MatrixIsAbelianGroup
     val vectorSpace = new MatrixIsVectorSpace
     val vsetLike = new MatrixIsSetVecLike
     val matrixLike = new MatrixIsMatrixLike
}


trait MatrixInstances {

     implicit final def matrixHasAbsoluteValue[N: Number] = new MatrixThings[N].absolute
     implicit final def matrixIsMonoid[N: Number] = new MatrixThings[N].monoid
     implicit final def matrixIsAbelianGroup[N: Number] = new MatrixThings[N].abelian
     implicit final def matrixIsVectorSpace[N: Number] = new MatrixThings[N].vectorSpace
     implicit final def matrixIsSetVecLike[N: Number] = new MatrixThings[N].vsetLike
     implicit final def matrixHasEq[N: Number] = new MatrixThings[N].eq
     implicit final def matrixHasDimension[N: Number] = new MatrixThings[N].dim
     implicit final def matrixIsMatrixLike[N: Number] = new MatrixThings[N].matrixLike


}

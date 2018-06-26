package linalg.instances.linear


import spire.algebra.Eq
import linalg.implicits._
import linalg._
import linalg.matrix.Matrix
import linalg.vector.{SetOfVectors, Vector}
import linalg.util._

import scala.language.higherKinds
import scala.language.implicitConversions




class MatrixThings[N: Number] {

     //TODO GOAL: implement matrixlike things without re-implementing them again, since similar implementations for
     // TODO for setveclike and matrix things.


     /*class MatrixHasAbsoluteValue extends Absolute[Matrix[N], Matrix[N]] {

          def absoluteValue(mat: Matrix[N]): Matrix[N] =
               Matrix(mat.getColumns().map(vec => vec.abs()):_*)
     }*/

     class MatrixHasDimension extends Dimension[Matrix[N]]{
          def dimension(mat: Matrix[N]): Int = Util.dimension(mat)
     }

     class MatrixHasEq extends Eq[Matrix[N]]{
          def eqv(mat1: Matrix[N], mat2: Matrix[N]): Boolean = Util.eqv(mat1, mat2)
     }

     class MatrixIsMonoid extends Monoid[Matrix[N]]{

          val zero: Matrix[N] = Matrix(Vector.ZERO[N](1))

          def plus(mat1: Matrix[N], mat2: Matrix[N]): Matrix[N] = Util.plus(mat1, mat2).toMatrix
     }

     class MatrixIsAbelianGroup extends MatrixIsMonoid with AbelianGroup[Matrix[N]]{
          def negate(mat: Matrix[N]): Matrix[N] = Util.negate(mat).toMatrix
     }

     class MatrixIsVectorSpace extends MatrixIsAbelianGroup with VectorSpace[Matrix[N], N]{
          val one: Matrix[N] = Matrix(Vector.ONE[N](1))

          def scale(mat: Matrix[N], factor: N): Matrix[N] = Util.scale(mat, factor).toMatrix
     }

     class MatrixIsSetVecLike extends MatrixIsVectorSpace with SetVecLike[Matrix[N], N]{
          def isZero(mat: Matrix[N]): Boolean = Util.isZero(mat)
          def identity(size: Int): Matrix[N] = Matrix(Util.identity(size).getColumns():_*)
          def rowEchelon(mat: Matrix[N]): Matrix[N] = Util.rowEchelon(mat).toMatrix
          def rowReducedEchelon(mat: Matrix[N]): Matrix[N] = Util.rowReducedEchelon(mat).toMatrix

     }

     class MatrixIsMatrixLike extends MatrixIsSetVecLike with MatrixLike[Matrix[N], N] {

          def power(mat: Matrix[N], exp: N): Matrix[N] = Util.power(mat, exp)
          def inverse(mat: Matrix[N]): Matrix[N] = Util.inverse(mat)
          def transpose(mat: Matrix[N]): Matrix[N] = Util.transpose(mat)
          def conjugateTranspose(mat: Matrix[N]): Matrix[N] = Util.conjugateTranspose(mat)
          def adjoint(mat: Matrix[N]): Matrix[N] =  Util.adjoint(mat)
          def cofactor(mat: Matrix[N]): Matrix[N] = Util.cofactor(mat)
          def cofactor(mat: Matrix[N], r:Int, c:Int): N = Util.cofactor(mat, r, c)
          def minor(mat: Matrix[N]): Matrix[N] = Util.minor(mat)
          def minor(mat: Matrix[N], r: Int, c: Int): N = Util.minor(mat, r, c)
          def determinant(mat: Matrix[N]): N = Util.determinant(mat)
          def trace(mat: Matrix[N]): N = Util.trace(mat)
     }



     //span, basis ... etc


     //val absolute = new MatrixHasAbsoluteValue
     val eq = new MatrixHasEq
     val dim = new MatrixHasDimension
     val monoid = new MatrixIsMonoid
     val abelian = new MatrixIsAbelianGroup
     val vectorSpace = new MatrixIsVectorSpace
     val setVecLike = new MatrixIsSetVecLike
     val matrixLike = new MatrixIsMatrixLike
}


trait MatrixInstances {

     //implicit final def matrixHasAbsoluteValue[N: Number] = new MatrixThings[N].absolute
     implicit final def matrixIsMonoid[N: Number] = new MatrixThings[N].monoid
     implicit final def matrixIsAbelianGroup[N: Number] = new MatrixThings[N].abelian
     implicit final def matrixIsVectorSpace[N: Number] = new MatrixThings[N].vectorSpace
     implicit final def matrixIsSetVecLike[N: Number] = new MatrixThings[N].setVecLike
     implicit final def matrixHasEq[N: Number] = new MatrixThings[N].eq
     implicit final def matrixHasDimension[N: Number] = new MatrixThings[N].dim
     implicit final def matrixIsMatrixLike[N: Number] = new MatrixThings[N].matrixLike


}

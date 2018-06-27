/*
package linalg.instances.linear

import spire.algebra.Eq
import linalg.implicits._
import linalg._
import linalg.matrix.{HilbertMatrix, Matrix}
import linalg.vector.{SetOfVectors, Vector}
import linalg.util._

import scala.language.higherKinds
import scala.language.implicitConversions

/**
  *
  */
  *
  *
  * //TODO Check over all these implementations - can't just do what we did for rref form, need
  * to consider hilbert as separate entity,????

class HilbertMatrixThings[N: Number] {

     /*class MatrixHasAbsoluteValue extends Absolute[Matrix[N], Matrix[N]] {

          def absoluteValue(mat: Matrix[N]): Matrix[N] =
               Matrix(mat.getColumns().map(vec => vec.abs()):_*)
     }*/

     class HilbertMatrixHasDimension extends Dimension[HilbertMatrix[N]]{
          def dimension(mat: HilbertMatrix[N]): Int = Util.dimension(mat)
     }

     class HilbertMatrixHasEq extends Eq[HilbertMatrix[N]]{
          def eqv(mat1: HilbertMatrix[N], mat2: HilbertMatrix[N]): Boolean = Util.eqv(mat1, mat2)
     }

     class HilbertMatrixIsMonoid extends Monoid[HilbertMatrix[N]]{

          val zero: HilbertMatrix[N] = HilbertMatrix(Vector.ZERO[N](1))
          def plus(mat1: HilbertMatrix[N], mat2: HilbertMatrix[N]): HilbertMatrix[N] =
               Util.plus(mat1, mat2).toHilbertMatrix
     }

     class HilbertMatrixIsAbelianGroup extends HilbertMatrixIsMonoid with AbelianGroup[HilbertMatrix[N]]{
          def negate(mat: HilbertMatrix[N]): HilbertMatrix[N] = Util.negate(mat).toHilbertMatrix
     }

     class HilbertMatrixIsVectorSpace extends HilbertMatrixIsAbelianGroup with VectorSpace[HilbertMatrix[N], N]{
          val one: HilbertMatrix[N] = HilbertMatrix(Vector.ONE[N](1))
          def scale(mat: HilbertMatrix[N], factor: N): HilbertMatrix[N] = Util.scale(mat, factor).toHilbertMatrix
     }

     class HilbertMatrixIsSetVecLike extends HilbertMatrixIsVectorSpace with SetVecLike[HilbertMatrix[N], N]{
          def isZero(mat: HilbertMatrix[N]): Boolean = Util.isZero(mat)
          def rowReducedEchelon(mat: HilbertMatrix[N]): HilbertMatrix[N] =
               Util.rowReducedEchelon(mat).toHilbertMatrix
          def rowEchelon(mat: HilbertMatrix[N]): HilbertMatrix[N] = Util.rowEchelon(mat).toHilbertMatrix
          def size(mat: HilbertMatrix[N]): (Int, Int) = Util.size(mat)
     }

     class HilbertMatrixIsMatrixLike extends HilbertMatrixIsSetVecLike with MatrixLike[HilbertMatrix[N], N] {

          def multiply(mat1: HilbertMatrix[N], mat2: HilbertMatrix[N]): HilbertMatrix[N] =
               Util.multiply(mat1, mat2).toHilbertMatrix
          def power(mat: HilbertMatrix[N], exp: N): HilbertMatrix[N] = Util.power(mat, exp).toHilbertMatrix
          def inverse(mat: HilbertMatrix[N]): HilbertMatrix[N] = Util.inverse(mat).toHilbertMatrix
          def transpose(mat: HilbertMatrix[N]): HilbertMatrix[N] = Util.transpose(mat).toHilbertMatrix
          def conjugateTranspose(mat: HilbertMatrix[N]): HilbertMatrix[N] = Util.conjugateTranspose(mat).toHilbertMatrix
          def adjoint(mat: HilbertMatrix[N]): HilbertMatrix[N] =  Util.adjoint(mat).toHilbertMatrix
          def cofactor(mat: HilbertMatrix[N]): HilbertMatrix[N] = Util.cofactor(mat).toHilbertMatrix
          def cofactor(mat: HilbertMatrix[N], r:Int, c:Int): N = Util.cofactor(mat, r, c)
          def minor(mat: HilbertMatrix[N]): HilbertMatrix[N] = Util.minor(mat).toHilbertMatrix
          def minor(mat: HilbertMatrix[N], r: Int, c: Int): N = Util.minor(mat, r, c)
          def determinant(mat: HilbertMatrix[N]): N = Util.determinant(mat)
          def trace(mat: HilbertMatrix[N]): N = Util.trace(mat)
     }



     //span, basis ... etc


     //val absolute = new MatrixHasAbsoluteValue
     val eq = new HilbertMatrixHasEq
     val dim = new HilbertMatrixHasDimension
     val monoid = new HilbertMatrixIsMonoid
     val abelian = new HilbertMatrixIsAbelianGroup
     val vectorSpace = new HilbertMatrixIsVectorSpace
     val setVecLike = new HilbertMatrixIsSetVecLike
     val matrixLike = new HilbertMatrixIsMatrixLike
}


trait HilbertMatrixInstances {

     //implicit final def matrixHasAbsoluteValue[N: Number] = new MatrixThings[N].absolute
     implicit final def hilbertMatrixIsMonoid[N: Number] = new HilbertMatrixThings[N].monoid
     implicit final def hilbertMatrixIsAbelianGroup[N: Number] = new HilbertMatrixThings[N].abelian
     implicit final def hilbertMatrixIsVectorSpace[N: Number] = new HilbertMatrixThings[N].vectorSpace
     implicit final def hilbertMatrixIsSetVecLike[N: Number] = new HilbertMatrixThings[N].setVecLike
     implicit final def hilbertMatrixHasEq[N: Number] = new HilbertMatrixThings[N].eq
     implicit final def hilbertMatrixHasDimension[N: Number] = new HilbertMatrixThings[N].dim
     implicit final def hilbertMatrixIsMatrixLike[N: Number] = new HilbertMatrixThings[N].matrixLike


}

*/

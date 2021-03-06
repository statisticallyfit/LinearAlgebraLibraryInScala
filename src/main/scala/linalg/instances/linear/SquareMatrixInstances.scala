
package linalg.instances.linear


import spire.algebra.Eq
import linalg.implicits._
import linalg._
import linalg.matrix.{SquareMatrix, Matrix}
import linalg.vector.{SetOfVectors, Vector}
import linalg.util._

import scala.language.higherKinds
import scala.language.implicitConversions




class SquareMatrixThings[N: Number] {

     /*class MatrixHasAbsoluteValue extends Absolute[Matrix[N], Matrix[N]] {

          def absoluteValue(mat: Matrix[N]): Matrix[N] =
               Matrix(mat.getColumns().map(vec => vec.abs()):_*)
     }*/

     class SquareMatrixHasDimension extends Dimension[SquareMatrix[N]]{
          def dimension(mat: SquareMatrix[N]): Int = Ops.dimension(mat)
     }

     class SquareMatrixHasEq extends Eq[SquareMatrix[N]]{
          def eqv(mat1: SquareMatrix[N], mat2: SquareMatrix[N]): Boolean = Ops.eqv(mat1, mat2)
     }

     class SquareMatrixIsMonoid extends Monoid[SquareMatrix[N]]{

          val zero: SquareMatrix[N] = SquareMatrix(Vector.ZERO[N](1))
          def plus(mat1: SquareMatrix[N], mat2: SquareMatrix[N]): SquareMatrix[N] =
               Ops.plus(mat1, mat2).toSquareMatrix
     }

     class SquareMatrixIsAbelianGroup extends SquareMatrixIsMonoid with AbelianGroup[SquareMatrix[N]]{
          def negate(mat: SquareMatrix[N]): SquareMatrix[N] = Ops.negate(mat).toSquareMatrix
     }

     class SquareMatrixIsVectorSpace extends SquareMatrixIsAbelianGroup with VectorSpace[SquareMatrix[N], N]{
          val one: SquareMatrix[N] = SquareMatrix(Vector.ONE[N](1))
          def scale(mat: SquareMatrix[N], factor: N): SquareMatrix[N] = Ops.scale(mat, factor).toSquareMatrix
     }

     class SquareMatrixIsSetVecLike extends SquareMatrixIsVectorSpace with SetVecLike[SquareMatrix[N], N]{
          def isZero(mat: SquareMatrix[N]): Boolean = Ops.isZero(mat)
          def rowReducedEchelon(mat: SquareMatrix[N]): SquareMatrix[N] =
               Ops.rowReducedEchelon(mat).toSquareMatrix
          def rowEchelon(mat: SquareMatrix[N]): SquareMatrix[N] = Ops.rowEchelon(mat).toSquareMatrix
          def size(mat: SquareMatrix[N]): (Int, Int) = Ops.size(mat)
          def transpose(mat: SquareMatrix[N]): SquareMatrix[N] = Ops.transpose(mat).toSquareMatrix
     }

     class SquareMatrixIsMatrixLike extends SquareMatrixIsSetVecLike with MatrixLike[SquareMatrix[N], N] {

          def multiply(mat1: SquareMatrix[N], mat2: SquareMatrix[N]): SquareMatrix[N] =
               Ops.multiply(mat1, mat2).toSquareMatrix
          def power(mat: SquareMatrix[N], exp: N): SquareMatrix[N] = Ops.power(mat, exp).toSquareMatrix
          def inverse(mat: SquareMatrix[N]): SquareMatrix[N] = Ops.inverse(mat).toSquareMatrix
          def conjugateTranspose(mat: SquareMatrix[N]): SquareMatrix[N] = Ops.conjugateTranspose(mat).toSquareMatrix
          def adjoint(mat: SquareMatrix[N]): SquareMatrix[N] =  Ops.adjoint(mat).toSquareMatrix
          def cofactor(mat: SquareMatrix[N]): SquareMatrix[N] = Ops.cofactor(mat).toSquareMatrix
          def cofactor(mat: SquareMatrix[N], r:Int, c:Int): N = Ops.cofactor(mat, r, c)
          def minor(mat: SquareMatrix[N]): SquareMatrix[N] = Ops.minor(mat).toSquareMatrix
          def minor(mat: SquareMatrix[N], r: Int, c: Int): N = Ops.minor(mat, r, c)
          def determinant(mat: SquareMatrix[N]): N = Ops.determinant(mat)
          def trace(mat: SquareMatrix[N]): N = Ops.trace(mat)
     }



     //span, basis ... etc


     //val absolute = new MatrixHasAbsoluteValue
     val eq = new SquareMatrixHasEq
     val dim = new SquareMatrixHasDimension
     val monoid = new SquareMatrixIsMonoid
     val abelian = new SquareMatrixIsAbelianGroup
     val vectorSpace = new SquareMatrixIsVectorSpace
     val setVecLike = new SquareMatrixIsSetVecLike
     val matrixLike = new SquareMatrixIsMatrixLike
}


trait SquareMatrixInstances {

     //implicit final def matrixHasAbsoluteValue[N: Number] = new MatrixThings[N].absolute
     implicit final def squareMatrixIsMonoid[N: Number] = new SquareMatrixThings[N].monoid
     implicit final def squareMatrixIsAbelianGroup[N: Number] = new SquareMatrixThings[N].abelian
     implicit final def squareMatrixIsVectorSpace[N: Number] = new SquareMatrixThings[N].vectorSpace
     implicit final def squareMatrixIsSetVecLike[N: Number] = new SquareMatrixThings[N].setVecLike
     implicit final def squareMatrixHasEq[N: Number] = new SquareMatrixThings[N].eq
     implicit final def squareMatrixHasDimension[N: Number] = new SquareMatrixThings[N].dim
     implicit final def squareMatrixIsMatrixLike[N: Number] = new SquareMatrixThings[N].matrixLike


}


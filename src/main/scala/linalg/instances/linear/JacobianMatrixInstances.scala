/*

package linalg.instances.linear


import spire.algebra.Eq
import linalg.implicits._
import linalg._
import linalg.matrix.{JacobianMatrix, Matrix}
import linalg.vector.{SetOfVectors, Vector}
import linalg.util._

import scala.language.higherKinds
import scala.language.implicitConversions




class JacobianMatrixThings[N: Number] {

     //TODO GOAL: implement matrixlike things without re-implementing them again, since similar implementations for
     // TODO for setveclike and matrix things.


     /*class MatrixHasAbsoluteValue extends Absolute[Matrix[N], Matrix[N]] {

          def absoluteValue(mat: Matrix[N]): Matrix[N] =
               Matrix(mat.getColumns().map(vec => vec.abs()):_*)
     }*/

     class JacobianMatrixHasDimension extends Dimension[JacobianMatrix[N]]{
          def dimension(mat: JacobianMatrix[N]): Int = Util.dimension(mat)
     }

     class JacobianMatrixHasEq extends Eq[JacobianMatrix[N]]{
          def eqv(mat1: JacobianMatrix[N], mat2: JacobianMatrix[N]): Boolean = Util.eqv(mat1, mat2)
     }

     class JacobianMatrixIsMonoid extends Monoid[JacobianMatrix[N]]{

          val zero: JacobianMatrix[N] = JacobianMatrix(Vector.ZERO[N](1))
          def plus(mat1: JacobianMatrix[N], mat2: JacobianMatrix[N]): JacobianMatrix[N] =
               Util.plus(mat1, mat2).toJacobianMatrix
     }

     class JacobianMatrixIsAbelianGroup extends JacobianMatrixIsMonoid with AbelianGroup[JacobianMatrix[N]]{
          def negate(mat: JacobianMatrix[N]): JacobianMatrix[N] = Util.negate(mat).toJacobianMatrix
     }

     class JacobianMatrixIsVectorSpace extends JacobianMatrixIsAbelianGroup with VectorSpace[JacobianMatrix[N], N]{
          val one: JacobianMatrix[N] = JacobianMatrix(Vector.ONE[N](1))
          def scale(mat: JacobianMatrix[N], factor: N): JacobianMatrix[N] = Util.scale(mat, factor).toJacobianMatrix
     }

     class JacobianMatrixIsSetVecLike extends JacobianMatrixIsVectorSpace with SetVecLike[JacobianMatrix[N], N]{
          def isZero(mat: JacobianMatrix[N]): Boolean = Util.isZero(mat)
          def rowReducedEchelon(mat: JacobianMatrix[N]): JacobianMatrix[N] =
               Util.rowReducedEchelon(mat).toJacobianMatrix

          def size(mat: JacobianMatrix[N]): (Int, Int) = Util.size(mat)
     }

     class JacobianMatrixIsMatrixLike extends JacobianMatrixIsSetVecLike with MatrixLike[JacobianMatrix[N], N] {

          def multiply(mat1: JacobianMatrix[N], mat2: JacobianMatrix[N]): JacobianMatrix[N] =
               Util.multiply(mat1, mat2).toJacobianMatrix
          def power(mat: JacobianMatrix[N], exp: N): JacobianMatrix[N] = Util.power(mat, exp).toJacobianMatrix
          def inverse(mat: JacobianMatrix[N]): JacobianMatrix[N] = Util.inverse(mat).toJacobianMatrix
          def transpose(mat: JacobianMatrix[N]): JacobianMatrix[N] = Util.transpose(mat).toJacobianMatrix
          def conjugateTranspose(mat: JacobianMatrix[N]): JacobianMatrix[N] = Util.conjugateTranspose(mat).toJacobianMatrix
          def adjoint(mat: JacobianMatrix[N]): JacobianMatrix[N] =  Util.adjoint(mat).toJacobianMatrix
          def cofactor(mat: JacobianMatrix[N]): JacobianMatrix[N] = Util.cofactor(mat).toJacobianMatrix
          def cofactor(mat: JacobianMatrix[N], r:Int, c:Int): N = Util.cofactor(mat, r, c)
          def minor(mat: JacobianMatrix[N]): JacobianMatrix[N] = Util.minor(mat).toJacobianMatrix
          def minor(mat: JacobianMatrix[N], r: Int, c: Int): N = Util.minor(mat, r, c)
          def determinant(mat: JacobianMatrix[N]): N = Util.determinant(mat)
          def trace(mat: JacobianMatrix[N]): N = Util.trace(mat)
     }



     //span, basis ... etc


     //val absolute = new MatrixHasAbsoluteValue
     val eq = new JacobianMatrixHasEq
     val dim = new JacobianMatrixHasDimension
     val monoid = new JacobianMatrixIsMonoid
     val abelian = new JacobianMatrixIsAbelianGroup
     val vectorSpace = new JacobianMatrixIsVectorSpace
     val setVecLike = new JacobianMatrixIsSetVecLike
     val matrixLike = new JacobianMatrixIsMatrixLike
}


trait JacobianMatrixInstances {

     //implicit final def matrixHasAbsoluteValue[N: Number] = new MatrixThings[N].absolute
     implicit final def jacobianMatrixIsMonoid[N: Number] = new JacobianMatrixThings[N].monoid
     implicit final def jacobianMatrixIsAbelianGroup[N: Number] = new JacobianMatrixThings[N].abelian
     implicit final def jacobianMatrixIsVectorSpace[N: Number] = new JacobianMatrixThings[N].vectorSpace
     implicit final def jacobianMatrixIsSetVecLike[N: Number] = new JacobianMatrixThings[N].setVecLike
     implicit final def jacobianMatrixHasEq[N: Number] = new JacobianMatrixThings[N].eq
     implicit final def jacobianMatrixHasDimension[N: Number] = new JacobianMatrixThings[N].dim
     implicit final def jacobianMatrixIsMatrixLike[N: Number] = new JacobianMatrixThings[N].matrixLike


}

*/

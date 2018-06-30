package linalg.instances.linear

import spire.algebra.Eq
import linalg.implicits._
import linalg._
import linalg.matrix.{AugmentedMatrix, Matrix, SquareMatrix}
import linalg.vector.{SetOfVectors, Vector}
import linalg.util._

import scala.collection.mutable.{ListBuffer, Seq}
import scala.language.higherKinds
import scala.language.implicitConversions
import scala.util.control.Breaks._
/**
  *
  */

class AugmentedMatrixThings[N: Number] {

     /*class MatrixHasAbsoluteValue extends Absolute[Matrix[N], Matrix[N]] {

          def absoluteValue(mat: Matrix[N]): Matrix[N] =
               Matrix(mat.getColumns().map(vec => vec.abs()):_*)
     }*/

     class AugmentedMatrixHasDimension extends Dimension[AugmentedMatrix[N]]{
          def dimension(mat: AugmentedMatrix[N]): Int = Ops.dimension(mat)
     }

     class AugmentedMatrixHasEq extends Eq[AugmentedMatrix[N]]{
          def eqv(mat1: AugmentedMatrix[N], mat2: AugmentedMatrix[N]): Boolean = Ops.eqv(mat1, mat2)
     }

     class AugmentedMatrixIsMonoid extends Monoid[AugmentedMatrix[N]]{

          val zero: AugmentedMatrix[N] = AugmentedMatrix(Vector.ZERO[N](1))
          def plus(mat1: AugmentedMatrix[N], mat2: AugmentedMatrix[N]): AugmentedMatrix[N] =
               Ops.plus(mat1, mat2).toAugMatrix
     }

     class AugmentedMatrixIsAbelianGroup extends AugmentedMatrixIsMonoid with AbelianGroup[AugmentedMatrix[N]]{
          def negate(mat: AugmentedMatrix[N]): AugmentedMatrix[N] = Ops.negate(mat).toAugMatrix
     }

     class AugmentedMatrixIsVectorSpace extends AugmentedMatrixIsAbelianGroup with VectorSpace[AugmentedMatrix[N], N]{
          val one: AugmentedMatrix[N] = AugmentedMatrix(Vector.ONE[N](1))
          def scale(mat: AugmentedMatrix[N], factor: N): AugmentedMatrix[N] = Ops.scale(mat, factor).toAugMatrix
     }

     class AugmentedMatrixIsSetVecLike extends AugmentedMatrixIsVectorSpace with SetVecLike[AugmentedMatrix[N], N]{
          def isZero(mat: AugmentedMatrix[N]): Boolean = Ops.isZero(mat)
          def rowEchelon(mat: AugmentedMatrix[N]): AugmentedMatrix[N] =
               AugmentedMatrix(mat.echelonA, mat.echelonB)
          def rowReducedEchelon(mat: AugmentedMatrix[N]): AugmentedMatrix[N] =
               AugmentedMatrix(mat.rrefA, mat.rrefB)

          def transpose(mat: AugmentedMatrix[N]): AugmentedMatrix[N] = Ops.transpose(mat).toAugMatrix
          def size(mat: AugmentedMatrix[N]): (Int, Int) = Ops.size(mat)
     }

     class AugmentedMatrixIsMatrixLike extends AugmentedMatrixIsSetVecLike with MatrixLike[AugmentedMatrix[N], N] {

          def multiply(mat1: AugmentedMatrix[N], mat2: AugmentedMatrix[N]): AugmentedMatrix[N] =
               Ops.multiply(mat1, mat2).toAugMatrix
          def power(mat: AugmentedMatrix[N], exp: N): AugmentedMatrix[N] = Ops.power(mat, exp).toAugMatrix
          def inverse(mat: AugmentedMatrix[N]): AugmentedMatrix[N] = Ops.inverse(mat).toAugMatrix
          def conjugateTranspose(mat: AugmentedMatrix[N]): AugmentedMatrix[N] = Ops.conjugateTranspose(mat).toAugMatrix
          def adjoint(mat: AugmentedMatrix[N]): AugmentedMatrix[N] =  Ops.adjoint(mat).toAugMatrix
          def cofactor(mat: AugmentedMatrix[N]): AugmentedMatrix[N] = Ops.cofactor(mat).toAugMatrix
          def cofactor(mat: AugmentedMatrix[N], r:Int, c:Int): N = Ops.cofactor(mat, r, c)
          def minor(mat: AugmentedMatrix[N]): AugmentedMatrix[N] = Ops.minor(mat).toAugMatrix
          def minor(mat: AugmentedMatrix[N], r: Int, c: Int): N = Ops.minor(mat, r, c)
          def determinant(mat: AugmentedMatrix[N]): N = Ops.determinant(mat)
          def trace(mat: AugmentedMatrix[N]): N = Ops.trace(mat)
     }


     class AugmentedMatrixIsLinearSystem extends AugmentedMatrixIsMatrixLike with LinearSystem[AugmentedMatrix[N], N]{


          def isInconsistent(mat: AugmentedMatrix[N]): Boolean = Ops.isInconsistent(mat)
          def hasUniqueSolution(mat: AugmentedMatrix[N]): Boolean = Ops.hasUniqueSolution(mat)
          def hasInfiniteSolutions(mat: AugmentedMatrix[N]): Boolean = Ops.hasInfiniteSolutions(mat)
          def infiniteSolutionSolver(mat: AugmentedMatrix[N]): Matrix[N] = Ops.infiniteSolutionSolver(mat)
          def solve(mat: AugmentedMatrix[N]): Option[Matrix[N]] = Ops.solve(mat)
     }

     //val absolute = new MatrixHasAbsoluteValue
     val eq = new AugmentedMatrixHasEq
     val dim = new AugmentedMatrixHasDimension
     val monoid = new AugmentedMatrixIsMonoid
     val abelian = new AugmentedMatrixIsAbelianGroup
     val vectorSpace = new AugmentedMatrixIsVectorSpace
     val setVecLike = new AugmentedMatrixIsSetVecLike
     val matrixLike = new AugmentedMatrixIsMatrixLike
     val linearSystem = new AugmentedMatrixIsLinearSystem
}


trait AugmentedMatrixInstances {

     //implicit final def matrixHasAbsoluteValue[N: Number] = new MatrixThings[N].absolute
     implicit final def augMatrixHasEq[N: Number] = new AugmentedMatrixThings[N].eq
     implicit final def augMatrixHasDimension[N: Number] = new AugmentedMatrixThings[N].dim
     implicit final def augMatrixIsMonoid[N: Number] = new AugmentedMatrixThings[N].monoid
     implicit final def augMatrixIsAbelianGroup[N: Number] = new AugmentedMatrixThings[N].abelian
     implicit final def augMatrixIsVectorSpace[N: Number] = new AugmentedMatrixThings[N].vectorSpace
     implicit final def augMatrixIsSetVecLike[N: Number] = new AugmentedMatrixThings[N].setVecLike
     implicit final def augMatrixIsMatrixLike[N: Number] = new AugmentedMatrixThings[N].matrixLike
     implicit final def augMatrixIsLinearSystem[N: Number] = new AugmentedMatrixThings[N].linearSystem

}


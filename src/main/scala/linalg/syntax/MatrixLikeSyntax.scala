package linalg.syntax

import linalg._

import scala.language.higherKinds
import scala.language.implicitConversions
/**
  *
  */
trait MatrixLikeSyntax extends SetVecLikeSyntax {

     implicit class MatrixLikeOps[M[_], N: Number](current: M[N])(implicit mat: MatrixLike[M[N], N]){

          def power(exp: N): M[N] = mat.power(current, exp)
          def inverse(): M[N] = mat.inverse(current)
          def transpose(): M[N] = mat.transpose(current)
          def conjugateTranspose(): M[N] = mat.conjugateTranspose(current)
          def adjoint(): M[N] = mat.adjoint(current)
          def cofactor(): M[N] = mat.cofactor(current)
          def minor(): M[N] = mat.minor(current)
          def minor(rowIndex: Int, colIndex: Int): N = mat.minor(current, rowIndex, colIndex)
          def determinant(): M[N] = mat.determinant(current)
          def trace(): N = mat.trace(current)
     }
}
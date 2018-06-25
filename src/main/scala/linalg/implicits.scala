package linalg

import linalg.kernel.{Rational, Real}
import linalg.vector.{SetOfVectors, Vector}
import linalg.matrix.{JacobianMatrix, Matrix}

import scala.language.implicitConversions

/**
  *
  */
object implicits extends syntax.AllSyntax with instances.AllInstances with GeneralImplicits

trait GeneralImplicits {
     //Converting Int to Rational
     implicit def intToRational(int: Int): Rational = Rational(int)
     // Converting Double To Real
     implicit def doubleToReal(double: Double): Real = Real(double)


     //converting setvec to matrix so that plus(vset, wset) works with matrices too (fine to
     // pass in matrices but returning setvec is wrong for a matrix
     implicit class SetVecToMatrix[N: Number](vset: SetOfVectors[N]) {
          def toMatrix: Matrix[N] = Matrix(vset.getColumns():_*)
     }

     implicit class SetVecToJacobian[N: Number](vset: SetOfVectors[N]) {
          def toJacobianMatrix: JacobianMatrix[N] = JacobianMatrix(vset.getColumns():_*)
     }

     /*implicit def matrixToJacobianMatrix[N:Number](mat: Matrix[N]): JacobianMatrix[N] =
          JacobianMatrix(mat.getColumns():_*)*/

     /*implicit def setVecToJacobian[N:Number](vset: SetOfVectors[N]): JacobianMatrix[N] =
          JacobianMatrix(vset.getColumns():_*)*/
}
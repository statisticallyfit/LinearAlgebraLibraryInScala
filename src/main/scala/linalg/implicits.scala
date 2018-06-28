package linalg

import linalg.kernel.{Rational, Real}
import linalg.vector.{SetOfVectors, Vector}
import linalg.matrix._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
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

     // so that vset * v and v * vset works
     implicit def vecToSetVec[N: Number](vec: Vector[N]): SetOfVectors[N] = SetOfVectors(vec)

     implicit class VectorImplicits[N:Number](v: Vector[N]) {
          def toListB: ListBuffer[N] = ListBuffer(v.getElements():_*)
          def toMatrix: Matrix[N] = Matrix(v)
     }

     implicit class SeqImplicits[N:Number](v: Seq[N]) {
          def toListB: ListBuffer[N] = ListBuffer(v:_*)
          def toVec: Vector[N] = Vector[N](v:_*)
          def toSeqM: mutable.Seq[N] = mutable.Seq(v:_*)
     }



     //converting setvec to matrix so that plus(vset, wset) works with matrices too (fine to
     // pass in matrices but returning setvec is wrong for a matrix
     implicit class SetVecToMatrix[N: Number](vset: SetOfVectors[N]) {
          def toMatrix: Matrix[N] = Matrix(vset.getColumns():_*)
          def toHilbertMatrix: HilbertMatrix[N] = HilbertMatrix(vset.getColumns():_*)
          /*def toJacobianMatrix: JacobianMatrix[N] = JacobianMatrix(vset.getColumns():_*)
          def toHessianMatrix: HessianMatrix[N] = HessianMatrix(vset.getColumns():_*)*/
          def toSquareMatrix: SquareMatrix[N] = SquareMatrix(vset.getColumns():_*)
          def toSimilarMatrix: SimilarMatrix[N] = SimilarMatrix(vset.getColumns():_*)
          def toAugMatrix: AugmentedMatrix[N] = AugmentedMatrix(vset.toMatrix) //so matrix B is zero
          //TODO def toHessenbergMatrix: HessenbergMatrix[N] = HessenbergMatrix(vset.getColumns():_*)
          //TODO hermitian, lowertri, uppertri, orthog
     }
}
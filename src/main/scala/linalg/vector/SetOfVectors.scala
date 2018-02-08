package linalg.vector

import linalg.numeric._
import linalg.theory._
import linalg.theory.space._

import scala.collection.mutable.ListBuffer

/**
  *
  */


trait SetOfVectorsLike[S, F] extends VectorSpace[S, F]{

     def minus(v: S, w: S): S = plus(v, negate(w))
     def isZero(v: S): Boolean

     def rowReducedEchelon(m: S): S
}

//
//object SetOfVectorsLike {
//
//     implicit def VectorSetIsVectorSetLike[N: Number: Trig: Equiv]
//     (implicit vector: VectorLike[Vector[N], SetOfVectors[N], N]) = new SetOfVectorsLike[SetOfVectors[N], N]{
//
//          import linalg.syntax.NumberSyntax._
//          import linalg.syntax.VectorLikeSyntax._
//
//          val zero: SetOfVectors[N] = SetOfVectors(Vector.ZERO[N](1))
//          val one: SetOfVectors[N] = SetOfVectors(Vector.ONE[N](1))
//
//          def plus(v: SetOfVectors[N], w: SetOfVectors[N]): SetOfVectors[N] =
//               SetOfVectors(v.columns.zip(w.columns).map(colPair => colPair._1 + colPair._2):_*) //vector.plus(pair._1, pair._2)
//
//          def negate(v: SetOfVectors[N]): SetOfVectors[N] = SetOfVectors(v.columns.map(col => col.negate()):_*)
//
//          def scale(v: SetOfVectors[N], factor: N): SetOfVectors[N] = SetOfVectors(v.columns.map(col => col.scale(factor)))
//
//          def isZero(v: SetOfVectors[N]): Boolean = v.columns.forall(col => col.isZero())
//
//          def rowReducedEchelon(v: SetOfVectors[N]): SetOfVectors[N] = ??? //todo
//     }
//}


class SetOfVectors[N: Number](val columns: Vector[N]*)


object SetOfVectors {

     def apply[N: Number](cols: Vector[N]*): SetOfVectors[N] = SetOfVectors(cols:_*)

     def ZERO[N: Number](numCols: Int, numRows: Int): SetOfVectors[N] =
          SetOfVectors.fromSequences(Seq.fill[N](numCols, numRows)(Number.ZERO[N]):_*)

     def ONE[N: Number](numCols: Int, numRows: Int): SetOfVectors[N] =
          SetOfVectors.fromSequences(Seq.fill[N](numCols, numRows)(Number.ONE[N]):_*)

     def IDENTITY[N: Number](size: Int): SetOfVectors[N] ={
          val list = ListBuffer.fill[N](size, size)(Number.ZERO[N])

          for(r <- 0 until size) {
               for(c <- 0 until size)
                    if(r == c)
                         list(r)(c) = Number.ONE[N]
          }
          SetOfVectors.fromSequences(list:_*)
     }

     def fromSequences[N: Number](seqs: Seq[N]*): SetOfVectors[N] =
          SetOfVectors(seqs.map(aSeq => Vector(aSeq:_*)):_*)

}
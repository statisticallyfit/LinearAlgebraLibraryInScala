package linalg.matrix

import linalg._
import linalg.implicits._
import linalg.vector.{SetOfVectors, Vector}
import linalg.util._

import scala.collection.mutable.ListBuffer

/**
  *
  */
class AugmentedMatrix[N: Number](val A: SetOfVectors[N], val B: SetOfVectors[N])
     extends Matrix[N](Ops.colCombine(A, B).getColumns():_*) {


     val rrefEntire: SetOfVectors[N] = Ops.rowReducedEchelon(this)
     val rrefA: SetOfVectors[N] = SetOfVectors(rrefEntire.getColumns().take(A.numCols):_*)
     val rrefB: SetOfVectors[N] = SetOfVectors(rrefEntire.getColumns().takeRight(B.numCols):_*)

     val echelonEntire: SetOfVectors[N] = Ops.rowEchelon(this)
     val echelonA: SetOfVectors[N] = SetOfVectors(echelonEntire.getColumns().take(A.numCols):_*)
     val echelonB: SetOfVectors[N] = SetOfVectors(echelonEntire.getColumns().takeRight(B.numCols):_*)

     override def toString: String = Show[AugmentedMatrix[N]].show(this)
}


object AugmentedMatrix {
     def apply[N: Number](A: SetOfVectors[N], B: SetOfVectors[N]): AugmentedMatrix[N] =
          new AugmentedMatrix(A, B)

     def apply[N: Number](A: SetOfVectors[N]): AugmentedMatrix[N] = {

          val largest: Int = List(A.numRows, A.numCols).max
          new AugmentedMatrix[N](A, SquareMatrix.ZERO[N](largest))
     }

     def apply[N: Number](A: SetOfVectors[N], b: Vector[N]): AugmentedMatrix[N] ={
          if(b.isRow()) b.toCol()
          new AugmentedMatrix[N](A, Matrix(b))
     }

     def apply[N: Number](cols: Vector[N]*): AugmentedMatrix[N] ={
          if(cols.length == 1){
               AugmentedMatrix(Matrix(cols:_*))
          } else { //if length is 2 or greater
               AugmentedMatrix(Matrix(cols.dropRight(1):_*), Matrix(cols.last))
          }
     }

     def apply[N: Number](nr: Int, nc: Int): AugmentedMatrix[N] = AugmentedMatrix.ZERO[N](nr, nc)

     def ZERO[N: Number](nrows: Int, ncols: Int): AugmentedMatrix[N] =
          AugmentedMatrix.fromSeqs(ListBuffer.fill[N](ncols, nrows)(Number.ZERO[N]): _*)

     def IDENTITY[N: Number](size: Int): AugmentedMatrix[N] = Ops.identity[N](size).toAugMatrix

     def fromSeqs[N: Number](seqs: Seq[N]*): AugmentedMatrix[N] = AugmentedMatrix(seqs.map(_.toVec):_*)

     //assume data is along column
     def fromSingleSeq[N: Number](numRows: Int, numCols: Int, seq: Seq[N]): AugmentedMatrix[N] =
          fromSeqs(seq.grouped(numCols).toList:_*) //.toList.map(_.toList)


     def fromSeq[N: Number](nr: Int, nc: Int, seq: Seq[N]): AugmentedMatrix[N] ={
          fromSeqs(seq.grouped(nc).toList.map(s => Seq(s:_*)):_*)
     }
}


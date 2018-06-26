package linalg.matrix


import linalg._
import linalg.implicits._
import linalg.util._
import linalg.vector.Vector

import scala.collection.mutable.ListBuffer

/**
  *
  */
class SimilarMatrix[N: Number](cols: Vector[N]*) extends Matrix[N](cols:_*)


object SimilarMatrix {
     def apply[N: Number](nr: Int, nc: Int): SimilarMatrix[N] = SimilarMatrix.ZERO[N](nr, nc)

     def apply[N: Number](cols: Vector[N]*): SimilarMatrix[N] = new SimilarMatrix(cols: _*)

     def ZERO[N: Number](nrows: Int, ncols: Int): SimilarMatrix[N] =
          SimilarMatrix.fromSeqs(ListBuffer.fill[N](ncols, nrows)(Number.ZERO[N]): _*)

     def IDENTITY[N: Number](size: Int): SimilarMatrix[N] = Util.identity[N](size).toSimilarMatrix

     def fromSeqs[N: Number](seqs: Seq[N]*): SimilarMatrix[N] = SimilarMatrix(seqs.map(aSeq => Vector(aSeq:_*)):_*)


     //assume data is along column
     def fromSingleSeq[N: Number](numRows: Int, numCols: Int, seq: Seq[N]): SimilarMatrix[N] =
          fromSeqs(seq.grouped(numCols).toList:_*) //.toList.map(_.toList)


     def fromSeq[N: Number](nr: Int, nc: Int, seq: Seq[N]): SimilarMatrix[N] ={
          fromSeqs(seq.grouped(nc).toList.map(s => Seq(s:_*)):_*)
     }
}
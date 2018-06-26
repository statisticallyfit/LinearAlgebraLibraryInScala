package linalg.matrix

import linalg._
import linalg.implicits._
import linalg.util._
import linalg.vector.Vector

import scala.collection.mutable.ListBuffer

/**
  *
  */
class HessianMatrix[N: Number](cols: Vector[N]*) extends Matrix[N](cols:_*)



object HessianMatrix {
     def apply[N: Number](nr: Int, nc: Int): HessianMatrix[N] = HessianMatrix.ZERO[N](nr, nc)

     def apply[N: Number](cols: Vector[N]*): HessianMatrix[N] = new HessianMatrix(cols: _*)

     def ZERO[N: Number](nrows: Int, ncols: Int): HessianMatrix[N] =
          HessianMatrix.fromSeqs(ListBuffer.fill[N](ncols, nrows)(Number.ZERO[N]): _*)

     def IDENTITY[N: Number](size: Int): HessianMatrix[N] = Util.identity[N](size).toHessianMatrix

     def fromSeqs[N: Number](seqs: Seq[N]*): HessianMatrix[N] = HessianMatrix(seqs.map(aSeq => Vector(aSeq:_*)):_*)


     //assume data is along column
     def fromSingleSeq[N: Number](numRows: Int, numCols: Int, seq: Seq[N]): HessianMatrix[N] =
          fromSeqs(seq.grouped(numCols).toList:_*) //.toList.map(_.toList)


     def fromSeq[N: Number](nr: Int, nc: Int, seq: Seq[N]): HessianMatrix[N] ={
          fromSeqs(seq.grouped(nc).toList.map(s => Seq(s:_*)):_*)
     }
}
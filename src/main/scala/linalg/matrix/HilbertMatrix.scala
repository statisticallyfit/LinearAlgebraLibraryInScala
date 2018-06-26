package linalg.matrix

import linalg._
import linalg.implicits._
import linalg.util._
import linalg.vector.Vector

import scala.collection.mutable.ListBuffer

/**
  *
  */
class HilbertMatrix[N: Number](cols: Vector[N]*) extends Matrix[N](cols:_*)



object HilbertMatrix {
     def apply[N: Number](nr: Int, nc: Int): HilbertMatrix[N] = HilbertMatrix.ZERO[N](nr, nc)

     def apply[N: Number](cols: Vector[N]*): HilbertMatrix[N] = new HilbertMatrix(cols: _*)

     def ZERO[N: Number](nrows: Int, ncols: Int): HilbertMatrix[N] =
          HilbertMatrix.fromSeqs(ListBuffer.fill[N](ncols, nrows)(Number.ZERO[N]): _*)

     def IDENTITY[N: Number](size: Int): HilbertMatrix[N] = Util.identity[N](size).toHilbertMatrix

     def fromSeqs[N: Number](seqs: Seq[N]*): HilbertMatrix[N] = HilbertMatrix(seqs.map(aSeq => Vector(aSeq:_*)):_*)


     //assume data is along column
     def fromSingleSeq[N: Number](numRows: Int, numCols: Int, seq: Seq[N]): HilbertMatrix[N] =
          fromSeqs(seq.grouped(numCols).toList:_*) //.toList.map(_.toList)


     def fromSeq[N: Number](nr: Int, nc: Int, seq: Seq[N]): HilbertMatrix[N] ={
          fromSeqs(seq.grouped(nc).toList.map(s => Seq(s:_*)):_*)
     }
}
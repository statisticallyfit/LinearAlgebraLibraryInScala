package linalg.matrix

import linalg._
import linalg.implicits._
import linalg.util._
import linalg.vector.{SetOfVectors, Vector}

import scala.collection.mutable.ListBuffer

/**
  *
  */
class JacobianMatrix[N: Number](cols: Vector[N]*) extends Matrix[N](cols:_*)



object JacobianMatrix {
     def apply[N: Number](nr: Int, nc: Int): JacobianMatrix[N] = JacobianMatrix.ZERO[N](nr, nc)

     def apply[N: Number](cols: Vector[N]*): JacobianMatrix[N] = new JacobianMatrix(cols: _*)

     def ZERO[N: Number](nrows: Int, ncols: Int): JacobianMatrix[N] =
          JacobianMatrix.fromSeqs(ListBuffer.fill[N](ncols, nrows)(Number.ZERO[N]): _*)

     def IDENTITY[N: Number](size: Int)(implicit ev: SetVecLike[JacobianMatrix[N],N]): JacobianMatrix[N] =
          ev.identity(size)

     def fromSeqs[N: Number](seqs: Seq[N]*): JacobianMatrix[N] = JacobianMatrix(seqs.map(aSeq => Vector(aSeq:_*)):_*)

     //assume data is along column
     def fromSingleSeq[N: Number](numRows: Int, numCols: Int, seq: Seq[N]): JacobianMatrix[N] =
          fromSeqs(seq.grouped(numCols).toList:_*) //.toList.map(_.toList)


     def fromSeq[N: Number](nr: Int, nc: Int, seq: Seq[N]): JacobianMatrix[N] ={
          fromSeqs(seq.grouped(nc).toList.map(s => Seq(s:_*)):_*)
     }
}
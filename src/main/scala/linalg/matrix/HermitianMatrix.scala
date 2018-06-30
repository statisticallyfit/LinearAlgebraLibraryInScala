package linalg.matrix

import linalg._
import linalg.implicits._
import linalg.util._
import linalg.vector.Vector

import scala.collection.mutable.ListBuffer

/**
  *
  */
class HermitianMatrix[N: Number](smat: SquareMatrix[N]) extends SquareMatrix[N](smat) {
     require(Ops.Id.isHermitian(smat))
}


object HermitianMatrix {

     def apply[N: Number](smat: SquareMatrix[N]): HermitianMatrix[N] = new HermitianMatrix[N](smat)

     def ZERO[N: Number](n:Int): HermitianMatrix[N] = new HermitianMatrix(SquareMatrix.ZERO[N](n))

     def IDENTITY[N: Number](size:Int): HermitianMatrix[N] = HermitianMatrix(SquareMatrix.IDENTITY[N](size))

     def fromSeqs[N: Number](seqs: Seq[N]*): HermitianMatrix[N] = HermitianMatrix(SquareMatrix.fromSeqs(seqs:_*))

     //assume data is along column
     def fromSingleSeq[N: Number](numRows: Int, numCols: Int, seq: Seq[N]): HermitianMatrix[N] =
          fromSeqs(seq.grouped(numCols).toList:_*) //.toList.map(_.toList)


     def fromSeq[N: Number](nr: Int, nc: Int, seq: Seq[N]): HermitianMatrix[N] ={
          fromSeqs(seq.grouped(nc).toList.map(s => Seq(s:_*)):_*)
     }
}

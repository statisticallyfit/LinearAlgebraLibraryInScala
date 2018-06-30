package linalg.matrix

import linalg.util._
import linalg._
import linalg.implicits._
import linalg.vector.SetOfVectors

/**
  *
  */
class UpperTriangularMatrix[N: Number](smat: SquareMatrix[N]) extends SquareMatrix[N](smat) {
     require(Ops.Id.isUpperTriangular(smat))
}


object UpperTriangularMatrix {

     def apply[N: Number](smat: SquareMatrix[N]): UpperTriangularMatrix[N] = new UpperTriangularMatrix[N](smat)

     def ZERO[N: Number](n:Int): UpperTriangularMatrix[N] = new UpperTriangularMatrix(SquareMatrix.ZERO[N](n))

     def IDENTITY[N: Number](size:Int): UpperTriangularMatrix[N] = UpperTriangularMatrix(SquareMatrix.IDENTITY[N](size))

     def fromSeqs[N: Number](seqs: Seq[N]*): UpperTriangularMatrix[N] = UpperTriangularMatrix(SquareMatrix.fromSeqs(seqs:_*))

     //assume data is along column
     def fromSingleSeq[N: Number](numRows: Int, numCols: Int, seq: Seq[N]): UpperTriangularMatrix[N] =
          fromSeqs(seq.grouped(numCols).toList:_*) //.toList.map(_.toList)


     def fromSeq[N: Number](nr: Int, nc: Int, seq: Seq[N]): UpperTriangularMatrix[N] ={
          fromSeqs(seq.grouped(nc).toList.map(s => Seq(s:_*)):_*)
     }
}
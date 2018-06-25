package linalg.matrix

import linalg._
import linalg.implicits._
import linalg.util._
/**
  *
  */
class UnitaryMatrix[N: Number](smat: SquareMatrix[N]) extends SquareMatrix[N](smat) {
     require(Util.Id.isUnitary(smat))
}


object UnitaryMatrix {

     def apply[N: Number](smat: SquareMatrix[N]): UnitaryMatrix[N] = new UnitaryMatrix[N](smat)

     def ZERO[N: Number](n:Int): UnitaryMatrix[N] = new UnitaryMatrix(SquareMatrix.ZERO[N](n))

     def IDENTITY[N: Number](size:Int): UnitaryMatrix[N] = UnitaryMatrix(SquareMatrix.IDENTITY[N](size))

     def fromSeqs[N: Number](seqs: Seq[N]*): UnitaryMatrix[N] = UnitaryMatrix(SquareMatrix.fromSeqs(seqs:_*))

     //assume data is along column
     def fromSingleSeq[N: Number](numRows: Int, numCols: Int, seq: Seq[N]): UnitaryMatrix[N] =
          fromSeqs(seq.grouped(numCols).toList:_*) //.toList.map(_.toList)


     def fromSeq[N: Number](nr: Int, nc: Int, seq: Seq[N]): UnitaryMatrix[N] ={
          fromSeqs(seq.grouped(nc).toList.map(s => Seq(s:_*)):_*)
     }
}
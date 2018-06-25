package linalg.matrix

import linalg._
import linalg.implicits._
import linalg.util._

/**
  *
  */
class OrthogonalMatrix[N: Number](smat: SquareMatrix[N]) extends SquareMatrix[N](smat) {
     require(Util.Id.isOrthogonal(smat))
}



object OrthogonalMatrix {

     def apply[N: Number](smat: SquareMatrix[N]): OrthogonalMatrix[N] = new OrthogonalMatrix[N](smat)

     def ZERO[N: Number](n:Int): OrthogonalMatrix[N] = new OrthogonalMatrix(SquareMatrix.ZERO[N](n))

     def IDENTITY[N: Number](size:Int): OrthogonalMatrix[N] = OrthogonalMatrix(SquareMatrix.IDENTITY[N](size))

     def fromSeqs[N: Number](seqs: Seq[N]*): OrthogonalMatrix[N] = OrthogonalMatrix(SquareMatrix.fromSeqs(seqs:_*))

     //assume data is along column
     def fromSingleSeq[N: Number](numRows: Int, numCols: Int, seq: Seq[N]): OrthogonalMatrix[N] =
          fromSeqs(seq.grouped(numCols).toList:_*) //.toList.map(_.toList)


     def fromSeq[N: Number](nr: Int, nc: Int, seq: Seq[N]): OrthogonalMatrix[N] ={
          fromSeqs(seq.grouped(nc).toList.map(s => Seq(s:_*)):_*)
     }
}

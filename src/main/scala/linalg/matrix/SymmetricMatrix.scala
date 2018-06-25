package linalg.matrix


import linalg._
import linalg.implicits._
import linalg.util._
import linalg.vector.Vector

/**
  *
  */
class SymmetricMatrix[N: Number](smat: SquareMatrix[N]) extends SquareMatrix[N](smat) {
     require(Util.Id.isSymmetric(smat))
}



object SymmetricMatrix {

     def apply[N: Number](smat: SquareMatrix[N]): SymmetricMatrix[N] = new SymmetricMatrix[N](smat)

     def ZERO[N: Number](n:Int): SymmetricMatrix[N] = new SymmetricMatrix(SquareMatrix.ZERO[N](n))

     def IDENTITY[N: Number](size:Int): SymmetricMatrix[N] = SymmetricMatrix(SquareMatrix.IDENTITY[N](size))

     def fromSeqs[N: Number](seqs: Seq[N]*): SymmetricMatrix[N] = SymmetricMatrix(SquareMatrix.fromSeqs(seqs:_*))

     //assume data is along column
     def fromSingleSeq[N: Number](numRows: Int, numCols: Int, seq: Seq[N]): SymmetricMatrix[N] =
          fromSeqs(seq.grouped(numCols).toList:_*) //.toList.map(_.toList)


     def fromSeq[N: Number](nr: Int, nc: Int, seq: Seq[N]): SymmetricMatrix[N] ={
          fromSeqs(seq.grouped(nc).toList.map(s => Seq(s:_*)):_*)
     }
}

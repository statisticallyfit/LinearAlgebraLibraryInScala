package linalg.matrix


import linalg._
import linalg.implicits._
import linalg.util._
import linalg.vector.Vector

import scala.collection.mutable.ListBuffer
/**
  *
  */
class LowerTriangularMatrix[N: Number](smat: SquareMatrix[N]) extends SquareMatrix[N](smat) {
     require(Ops.Id.isLowerTriangular(smat))
}


object LowerTriangularMatrix {

     def apply[N: Number](smat: SquareMatrix[N]): LowerTriangularMatrix[N] = new LowerTriangularMatrix[N](smat)

     def ZERO[N: Number](n:Int): LowerTriangularMatrix[N] = new LowerTriangularMatrix(SquareMatrix.ZERO[N](n))

     def IDENTITY[N: Number](size:Int): LowerTriangularMatrix[N] = LowerTriangularMatrix(SquareMatrix.IDENTITY[N](size))

     def fromSeqs[N: Number](seqs: Seq[N]*): LowerTriangularMatrix[N] = LowerTriangularMatrix(SquareMatrix.fromSeqs(seqs:_*))

     //assume data is along column
     def fromSingleSeq[N: Number](numRows: Int, numCols: Int, seq: Seq[N]): LowerTriangularMatrix[N] =
          fromSeqs(seq.grouped(numCols).toList:_*) //.toList.map(_.toList)


     def fromSeq[N: Number](nr: Int, nc: Int, seq: Seq[N]): LowerTriangularMatrix[N] ={
          fromSeqs(seq.grouped(nc).toList.map(s => Seq(s:_*)):_*)
     }
}
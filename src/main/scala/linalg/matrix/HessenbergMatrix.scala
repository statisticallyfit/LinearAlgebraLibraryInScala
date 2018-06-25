package linalg.matrix


import linalg._
import linalg.implicits._
import linalg.util._
/**
  *
  */
class HessenbergMatrix[N : Number](smat: SquareMatrix[N])
     extends SquareMatrix[N](Util.Trans.HessenbergTransformer.makeHessenberg(smat))



object HessenbergMatrix {

     def apply[N: Number](smat: SquareMatrix[N]): HessenbergMatrix[N] = new HessenbergMatrix[N](smat)

     def ZERO[N: Number](n:Int): HessenbergMatrix[N] = new HessenbergMatrix(SquareMatrix.ZERO[N](n))

     def IDENTITY[N: Number](size:Int): HessenbergMatrix[N] = HessenbergMatrix(SquareMatrix.IDENTITY[N](size))

     def fromSeqs[N: Number](seqs: Seq[N]*): HessenbergMatrix[N] = HessenbergMatrix(SquareMatrix.fromSeqs(seqs:_*))

     //assume data is along column
     def fromSingleSeq[N: Number](numRows: Int, numCols: Int, seq: Seq[N]): HessenbergMatrix[N] =
          fromSeqs(seq.grouped(numCols).toList:_*) //.toList.map(_.toList)


     def fromSeq[N: Number](nr: Int, nc: Int, seq: Seq[N]): HessenbergMatrix[N] ={
          fromSeqs(seq.grouped(nc).toList.map(s => Seq(s:_*)):_*)
     }
}

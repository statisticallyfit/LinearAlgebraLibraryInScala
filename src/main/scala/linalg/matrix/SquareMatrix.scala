package linalg.matrix

import linalg._
import linalg.implicits._
import linalg.vector.{Vector, SetOfVectors}
import linalg.util._

import scala.collection.mutable.ListBuffer

/**
  *
  */

//TODO: ssquarematrix implements setveclike using hlist shapeless so that no need
// for extending from Matrix (no effect, anyway)
class SquareMatrix[N: Number](mat: Matrix[N]) extends Matrix[N](mat.getColumns():_*) {
     require(Util.Id.isSquare(mat))
}


object SquareMatrix {

     def apply[N: Number](n:Int): SquareMatrix[N] =
          new SquareMatrix(Matrix.ZERO[N](n, n))

     def apply[N: Number](mat: Matrix[N]): SquareMatrix[N] = new SquareMatrix(mat)

     def apply[N: Number](cols: Vector[N]*): SquareMatrix[N] = new SquareMatrix(Matrix(cols:_*))

     def ZERO[N: Number](n:Int): SquareMatrix[N] = new SquareMatrix(Matrix.ZERO[N](n, n))

     def IDENTITY[N: Number](size:Int): SquareMatrix[N] = new SquareMatrix(Matrix.IDENTITY[N](size))

     def fromSeqs[N: Number](seqs: Seq[N]*): SquareMatrix[N] = SquareMatrix(seqs.map(aSeq => Vector(aSeq:_*)):_*)

     //assume data is along column
     def fromSingleSeq[N: Number](numRows: Int, numCols: Int, seq: Seq[N]): SquareMatrix[N] =
          fromSeqs(seq.grouped(numCols).toList:_*) //.toList.map(_.toList)


     def fromSeq[N: Number](nr: Int, nc: Int, seq: Seq[N]): SquareMatrix[N] ={
          fromSeqs(seq.grouped(nc).toList.map(s => Seq(s:_*)):_*)
     }
}


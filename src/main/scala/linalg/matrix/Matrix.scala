package linalg.matrix

import linalg.implicits._
import linalg._
import linalg.instances.linear.JacobianMatrixThings
import linalg.vector.{SetOfVectors,  Vector}

import scala.collection.mutable.ListBuffer




//TODO get rid of 'extends' by using Hlist shapeless
class Matrix[N: Number](private val cols: Vector[N]*) extends SetOfVectors[N](cols:_*)



object Matrix {
     def apply[N: Number](nr: Int, nc: Int): Matrix[N] = Matrix.ZERO[N](nr, nc)

     def apply[N: Number](cols: Vector[N]*): Matrix[N] = new Matrix(cols: _*)

     def ZERO[N: Number](nrows: Int, ncols: Int): Matrix[N] =
          Matrix.fromSeqs(ListBuffer.fill[N](ncols, nrows)(Number.ZERO[N]): _*)

     def IDENTITY[N: Number](mat: SetOfVectors[N]): Matrix[N] = {
          val largestSize: Int = List(mat.numRows, mat.numCols).max
          IDENTITY[N](largestSize)
     }

     def IDENTITY[N: Number](size: Int)(implicit ev: SetVecLike[Matrix[N], N]): Matrix[N] =
          ev.identity(size)

     def fromSeqs[N: Number](seqs: Seq[N]*): Matrix[N] = Matrix(seqs.map(aSeq => Vector(aSeq:_*)):_*)


     //assume data is along column
     def fromSingleSeq[N: Number](numRows: Int, numCols: Int, seq: Seq[N]): Matrix[N] =
          fromSeqs(seq.grouped(numCols).toList:_*) //.toList.map(_.toList)


     def fromSeq[N: Number](nr: Int, nc: Int, seq: Seq[N]): Matrix[N] ={
          fromSeqs(seq.grouped(nc).toList.map(s => Seq(s:_*)):_*)
     }
     /*def fromBuffers[N: Number](buffs: ListBuffer[N]*): Matrix[N] =
          new Matrix(buffs.map(list => new Vector(list: _*)): _*)

     }*/
}


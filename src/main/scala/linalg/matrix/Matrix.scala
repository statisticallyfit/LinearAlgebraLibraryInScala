package linalg.matrix

import linalg.implicits._
import linalg._
import linalg.util.Ops
import linalg.vector.{SetOfVectors, Vector}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer




//TODO get rid of 'extends' by using Hlist shapeless
class Matrix[N: Number](private val cols: Vector[N]*) extends SetOfVectors[N](cols:_*)



object Matrix {
     def apply[N: Number](nr: Int, nc: Int): Matrix[N] = Matrix.ZERO[N](nr, nc)

     def apply[N: Number](cols: Vector[N]*): Matrix[N] = new Matrix(cols: _*)

     def ZERO[N: Number](nrows: Int, ncols: Int): Matrix[N] =
          Matrix.fromSeqs(ListBuffer.fill[N](ncols, nrows)(Number.ZERO[N]): _*)

     def IDENTITY[N: Number](mat: SetOfVectors[N]): Matrix[N] = {
          //val largestSize: Int = List(mat.numRows, mat.numCols).max
          IDENTITY[N](mat.dimension())
     }

     def IDENTITY[N: Number](size: Int): Matrix[N] = Ops.identity[N](size).toMatrix

     def fromSeqs[N: Number](seqs: Seq[N]*): Matrix[N] = Matrix(seqs.map(_.toVec):_*)


     //assume data is along column
     /*def fromSingleSeq[N: Number](numRows: Int, numCols: Int, seq: Seq[N]): Matrix[N] =
          fromSeqs(seq.grouped(numCols).toList:_*) //.toList.map(_.toList)*/


     def fromFlat[N: Number](nr: Int, nc: Int, seq: Seq[N]): Matrix[N] ={
          fromSeqs(seq.grouped(nc).toList.map(s => Seq(s:_*)):_*)
     }

     def fromFlat[N: Number](nr: Int, nc: Int, seq: mutable.Seq[N]): Matrix[N] ={
          fromSeqs(seq.grouped(nc).toList.map(s => Seq(s:_*)):_*)
     }

     //assumes it is a sequence of vectors
     def fromSeq[N: Number](seq: Seq[Vector[N]]): Matrix[N] = new Matrix(seq:_*)
     /*def fromBuffers[N: Number](buffs: ListBuffer[N]*): Matrix[N] =
          new Matrix(buffs.map(list => new Vector(list: _*)): _*)

     }*/
}


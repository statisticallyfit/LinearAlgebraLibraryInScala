package linalg.vector


import linalg.implicits._
import linalg._
import linalg.util._

import scala.language.higherKinds
import scala.language.implicitConversions

import scala.collection.mutable.Seq

/**
  *
  */

//todo question - if you extend Matrix with setvec then is Matrix counted as implementing
// the setveclike trait? Need to get its methods as well as class setvec methods so as
//not to implement all over again. Same thing with poly-vector.

class SetOfVectors[N: Number](private val cols: Vector[N]*) {

     private val columns: Seq[Vector[N]] = Seq(cols:_*)
     val numRows: Int = columns.head.dimension()
     val numCols: Int = columns.length

     def copy(): SetOfVectors[N] = SetOfVectors(columns:_*)
     def copy(cols: Seq[Vector[N]]): SetOfVectors[N] = SetOfVectors(cols:_*)

     def getColumnsSeq(): Seq[Seq[N]] = Seq(columns.map(vec => vec.getElements()):_*)
     def getRowsSeq(): Seq[Seq[N]] = Seq(getRows().map(vec => vec.getElements()):_*)

     def getColumns(): Seq[Vector[N]] = Seq(columns:_*)
     def getColumn(colIndex: Int): Vector[N] = columns(colIndex)

     def getRow(rowIndex: Int): Vector[N] = {
          val rows = this.getRows()
          rows(rowIndex)
     }

     /**
       * Returns all rows
       */
     def getRows(): Seq[Vector[N]] = {
          //val rows: Seq[Vector[N]] = Seq()
          //for(r <- 0 until this.numRows) rows(r) = Vector(columns.map(colVec => colVec.get(r)):_*)
          //rows
          Util.expressColsAsRows(columns)
     }

     /**
       * Gets Rows at particular indices and returns them expressed as rows.
       */
     def getRowsAt(indices: Int*): Seq[Vector[N]] ={
          val allRows = this.getRows()
          val someRows = for(i <- indices) yield allRows(i)
          Seq(someRows:_*)
     }

     def getColumnsAt(indices: Int*): Seq[Vector[N]] = this.toMatrix.transpose().getRowsAt(indices:_*)

     def setColumn(colIndex: Int, col: Vector[N]): Unit = columns(colIndex) = col
     def setRow(rowIndex: Int, row: Vector[N]): Unit = {
          for(c <- 0 until numCols){
               this.set(rowIndex, c)(row.get(c))
          }
     }

     def get(rowIndex: Int, colIndex: Int): N = this.getRow(rowIndex).get(colIndex)
     def set(rowIndex: Int, colIndex:Int)(value: N): Unit = columns(colIndex).set(rowIndex)(value)


     override def toString: String = Show[SetOfVectors[N]].show(this)
}



object SetOfVectors {

     def apply[N: Number](cols: Vector[N]*): SetOfVectors[N] = new SetOfVectors(cols:_*)

     def apply[N: Number](nr:Int, nc:Int): SetOfVectors[N] =
          new SetOfVectors(Vector(Seq.fill[N](nr * nc)(Number[N].zero):_*))

     def ZERO[N: Number](numCols: Int, numRows: Int): SetOfVectors[N] =
          SetOfVectors.fromSeqs(Seq.fill[N](numCols, numRows)(Number[N].zero):_*)

     def ZERO[N: Number](n: Int): SetOfVectors[N] = ZERO(n, n)

     def ONE[N: Number](numCols: Int, numRows: Int): SetOfVectors[N] =
          SetOfVectors.fromSeqs(Seq.fill[N](numCols, numRows)(Number[N].one):_*)

     def IDENTITY[N: Number](size: Int):SetOfVectors[N] = Util.identity[N](size)

     def IDENTITY[N: Number](vset: SetOfVectors[N]): SetOfVectors[N] ={
          val largestSize: Int = List(vset.numRows, vset.numCols).max
          IDENTITY[N](largestSize)
     }

     def fromSeqs[N: Number](seqs: Seq[N]*): SetOfVectors[N] = SetOfVectors(seqs.map(aSeq => Vector(aSeq:_*)):_*)

     //assume data is along column
     def fromSingleSeq[N: Number](numRows: Int, numCols: Int, seq: Seq[N]): SetOfVectors[N] =
          fromSeqs(seq.grouped(numCols).toList:_*) //.toList.map(_.toList)

}

package linalg.vector


import linalg.implicits._
import linalg._

import scala.language.higherKinds
import scala.language.implicitConversions
import scala.collection.mutable.{ListBuffer, Seq}

/**
  *
  */

//todo question - if you extend Matrix with setvec then is Matrix counted as implementing
// the setveclike trait? Need to get its methods as well as class setvec methods so as
//not to implement all over again. Same thing with poly-vector.

class SetOfVectors[N: Number](private val cols: Vector[N]*) {

     private val columns: Seq[Vector[N]] = Seq(cols:_*)
     val numRows: Int = columns.head.dimension()
     //note is recursive here since goes back to instances file: Dimension[SetOfVectors[N]].dimension(this)
     val numCols: Int = columns.length

     def copy(): SetOfVectors[N] = SetOfVectors(columns:_*)
     def copy(cols: Seq[Vector[N]]): SetOfVectors[N] = SetOfVectors(cols:_*)

     def getColumnsSeq(): Seq[Seq[N]] = Seq(columns.map(vec => vec.getElements()):_*)
     def getColumns(): Seq[Vector[N]] = Seq(columns:_*)
     def getColumn(colIndex: Int): Vector[N] = columns(colIndex)

     def getRow(rowIndex: Int): Vector[N] = this.getRows()(rowIndex)
     def getRows(): Seq[Vector[N]] = {
          val rows: Seq[Vector[N]] = Seq()
          for(r <- 0 until this.numRows) rows(r) = Vector(columns.map(colVec => colVec.get(r)):_*)
          rows
     }

     def setColumn(colIndex: Int, col: Vector[N]): Unit = columns(colIndex) = col
     def setRow(rowIndex: Int, row: Vector[N]): Unit = {
          for(c <- 0 until numCols){
               this.set(rowIndex, c)(row.get(c))
          }
     }

     def get(rowIndex: Int, colIndex: Int): N = this.getRow(rowIndex).get(colIndex)
     def set(rowIndex: Int, colIndex:Int)(value: N): Unit = columns(colIndex).set(rowIndex)(value)


     override def toString: String = Show[SetOfVectors[N]].show(this)
     //TODO why 'could not find implicit value for param ev in linalg.kernel.Show??
          //Show[SetOfVectors[N]].show(this)
}



object SetOfVectors {

     def apply[N: Number](cols: Vector[N]*): SetOfVectors[N] = new SetOfVectors(cols:_*)

     def apply[N: Number](nr:Int, nc:Int): SetOfVectors[N] =
          new SetOfVectors(Vector(Seq.fill[N](nr * nc)(Number.ZERO[N]):_*))

     def ZERO[N: Number](numCols: Int, numRows: Int): SetOfVectors[N] =
          SetOfVectors.fromSeqs(Seq.fill[N](numCols, numRows)(Number.ZERO[N]):_*)

     def ONE[N: Number](numCols: Int, numRows: Int): SetOfVectors[N] =
          SetOfVectors.fromSeqs(Seq.fill[N](numCols, numRows)(Number.ONE[N]):_*)

     def IDENTITY[N: Number](size: Int)(implicit ev: SetVecLike[SetOfVectors[N], N]):SetOfVectors[N] =
          ev.identity(size)

     def IDENTITY[N: Number](vset: SetOfVectors[N])(implicit ev: SetVecLike[SetOfVectors[N], N]): SetOfVectors[N] =
          ev.identity(vset.dimension())

     def fromSeqs[N: Number](seqs: Seq[N]*): SetOfVectors[N] = SetOfVectors(seqs.map(aSeq => Vector(aSeq:_*)):_*)

     //assume data is along column
     def fromSingleSeq[N: Number](numRows: Int, numCols: Int, seq: Seq[N]): SetOfVectors[N] =
          fromSeqs(seq.grouped(numCols).toList:_*) //.toList.map(_.toList)

}

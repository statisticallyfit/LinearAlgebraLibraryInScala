//package linalg.vector
//
//import linalg._
//import linalg.implicits._
//import linalg.util._
//
//import scala.collection.mutable.Seq
//
///**
//  *
//  */
//private[linalg] class VSet[N: Number](private val cols: Vector[N]*) {
//
//     private val columns: Seq[Vector[N]] = Seq(cols:_*)
//     val numRows: Int = columns.head.dimension()
//     val numCols: Int = columns.length
//
//     def copy(): SetOfVectors[N] = SetOfVectors(columns:_*)
//     def copy(cols: Seq[Vector[N]]): SetOfVectors[N] = SetOfVectors(cols:_*)
//
//     def getColumnsSeq(): Seq[Seq[N]] = Seq(columns.map(vec => vec.getElements()):_*)
//     def getColumns(): Seq[Vector[N]] = Seq(columns:_*)
//     def getColumn(colIndex: Int): Vector[N] = columns(colIndex)
//
//     def getRow(rowIndex: Int): Vector[N] = {
//          val rows = this.getRows()
//          rows(rowIndex)
//     }
//
//     /**
//       * Returns all rows
//       */
//     def getRows(): Seq[Vector[N]] = {
//          //val rows: Seq[Vector[N]] = Seq()
//          //for(r <- 0 until this.numRows) rows(r) = Vector(columns.map(colVec => colVec.get(r)):_*)
//          //rows
//          Util.expressColsAsRows(columns)
//     }
//
//     /**
//       * Gets Rows at particular indices and returns them expressed as rows.
//       */
//     def getRowsAt(indices: Int*): Seq[Vector[N]] ={
//          val allRows = this.getRows()
//          val someRows = for(i <- indices) yield allRows(i)
//          Seq(someRows:_*)
//     }
//
//     def setColumn(colIndex: Int, col: Vector[N]): Unit = columns(colIndex) = col
//     def setRow(rowIndex: Int, row: Vector[N]): Unit = {
//          for(c <- 0 until numCols){
//               this.set(rowIndex, c)(row.get(c))
//          }
//     }
//
//     def get(rowIndex: Int, colIndex: Int): N = this.getRow(rowIndex).get(colIndex)
//     def set(rowIndex: Int, colIndex:Int)(value: N): Unit = columns(colIndex).set(rowIndex)(value)
//
//
//     override def toString: String = Show[VSet[N]].show(this)
//}
//
//
//object VSet {
//
//     def apply[N: Number](cols: Vector[N]*): VSet[N] = new VSet(cols:_*)
//
//     def apply[N: Number](nr:Int, nc:Int): VSet[N] =
//          new VSet(Vector(Seq.fill[N](nr * nc)(Number[N].zero):_*))
//
//     def ZERO[N: Number](numCols: Int, numRows: Int): VSet[N] =
//          VSet.fromSeqs(Seq.fill[N](numCols, numRows)(Number[N].zero):_*)
//
//     def ONE[N: Number](numCols: Int, numRows: Int): VSet[N] =
//          VSet.fromSeqs(Seq.fill[N](numCols, numRows)(Number[N].one):_*)
//
//     def IDENTITY[N: Number](size: Int)(implicit ev: SetVecLike[VSet[N], N]):VSet[N] =
//          ev.identity(size)
//
//     def IDENTITY[N: Number](vset: VSet[N])(implicit ev: SetVecLike[VSet[N], N]): VSet[N] =
//          ev.identity(vset.numCols)
//
//     def fromSeqs[N: Number](seqs: Seq[N]*): VSet[N] = VSet(seqs.map(aSeq => Vector(aSeq:_*)):_*)
//
//     //assume data is along column
//     def fromSingleSeq[N: Number](numRows: Int, numCols: Int, seq: Seq[N]): VSet[N] =
//          fromSeqs(seq.grouped(numCols).toList:_*) //.toList.map(_.toList)
//
//}
package linalg.matrix

import linalg._
import linalg.implicits._
import linalg.vector.Vector
import linalg.util._

import scala.collection.mutable.ListBuffer

/**
  *
  */
class AugmentedMatrix[N: Number](val A: Matrix[N], val B: Matrix[N])
     extends Matrix[N](Util.colCombine(A, B).getColumns():_*) {


     val rrefAll: AugmentedMatrix[N] = Util.rowReducedEchelon(this).toAugMatrix
     val rrefA: Matrix[N] = A.rowReducedEchelon() //note implicit vecset => matrix
     val rrefB: Matrix[N] = Matrix(rrefAll.getColumns().takeRight(B.numCols):_*)

}


object AugmentedMatrix {
     def apply[N: Number](A: Matrix[N], B: Matrix[N]): AugmentedMatrix[N] =
          new AugmentedMatrix(A, B)

     def apply[N: Number](A: Matrix[N]): AugmentedMatrix[N] = {

          val largest: Int = List(A.numRows, A.numCols).max
          new AugmentedMatrix[N](A, SquareMatrix.ZERO[N](largest))
     }

     def apply[N: Number](A: Matrix[N], b: Vector[N]): AugmentedMatrix[N] =
          new AugmentedMatrix[N](A, Matrix(b))

     def apply[N: Number](cols: Vector[N]*): AugmentedMatrix[N] =
          new AugmentedMatrix[N](Matrix(cols.dropRight(1):_*), Matrix(cols.last))

     def apply[N: Number](nr: Int, nc: Int): AugmentedMatrix[N] = AugmentedMatrix.ZERO[N](nr, nc)

     def ZERO[N: Number](nrows: Int, ncols: Int): AugmentedMatrix[N] =
          AugmentedMatrix.fromSeqs(ListBuffer.fill[N](ncols, nrows)(Number.ZERO[N]): _*)

     def IDENTITY[N: Number](size: Int)(implicit ev: SetVecLike[AugmentedMatrix[N],N]): AugmentedMatrix[N] =
          ev.identity(size)

     def fromSeqs[N: Number](seqs: Seq[N]*): AugmentedMatrix[N] = AugmentedMatrix(seqs.map(aSeq => Vector(aSeq:_*)):_*)

     //assume data is along column
     def fromSingleSeq[N: Number](numRows: Int, numCols: Int, seq: Seq[N]): AugmentedMatrix[N] =
          fromSeqs(seq.grouped(numCols).toList:_*) //.toList.map(_.toList)


     def fromSeq[N: Number](nr: Int, nc: Int, seq: Seq[N]): AugmentedMatrix[N] ={
          fromSeqs(seq.grouped(nc).toList.map(s => Seq(s:_*)):_*)
     }
}

//
//
//
//     override def toString: String ={
//          val colsStr: ListBuffer[List[String]] = this.getColumns().map(vec => vec.toList.map(elem => elem
//               .toString))
//
//          // max widths measured per col
//          val maxWidths: ListBuffer[Int] = colsStr.map(vec => vec.reduceLeft((acc,y) =>
//               if(acc.length > y.length) acc else y)).map(_.length)
//
//          val maxWidthsTwoDim: ListBuffer[List[Int]] = maxWidths.map(elem => List.fill(numRows)(elem))
//
//          // col center length tupled with actual matrix col element in vector of vectors
//          val pairs = colsStr.zip(maxWidthsTwoDim).map(pair => pair._1.zip(pair._2))
//          val alignedCols/*: ListBuffer[List[String]]*/ = pairs.map(vec =>
//               vec.map(pair => StringUtils.leftPad(pair._1.toString, pair._2)))
//
//          var sepAlignedCols = alignedCols.take(A.numCols) += List.fill[String](A.numRows)("|")
//          sepAlignedCols = sepAlignedCols ++ alignedCols.drop(A.numCols)
//
//          // note: let maxWidth + 2 separate the numbers in the row
//          if(numRows == 1)
//               return "\n{" + sepAlignedCols.transpose.head.mkString("  ") + "}"
//          val firstRow: String = "\n/ " + sepAlignedCols.transpose.head.mkString("  ") + " \\\n"
//          val lastRow: String = "\\ " + sepAlignedCols.transpose.last.mkString("  ") + " /"
//          val middleRows: ListBuffer[String] = sepAlignedCols.transpose.tail.init
//               .map(list => "| " + list.mkString("  ") + " |\n")
//
//          firstRow + middleRows.mkString + lastRow
//     }
//}
//
//

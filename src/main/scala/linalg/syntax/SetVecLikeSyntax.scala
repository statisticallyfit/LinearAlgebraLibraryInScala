package linalg.syntax

import cats.Eq

import linalg.numeric._
import linalg.numeric.Number._
import linalg.vector._
import linalg.vector.VectorLike._

import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions
import scala.language.higherKinds

/**
  *
  */


object SetVecLikeSyntax {

     implicit class SetVecLikeOps[S[_],
          N: Number: Trig: Root: Absolute: Compare](current: S[N])(implicit ev: SetVecLike[S[N], N]){

          def rowReducedEchelon(): S[N] = ev.rowReducedEchelon(current)

          def minus(other: S[N]): S[N] = ev.minus(current, other)
          def isZero: Boolean = ev.isZero(current)

          def numRows: Int = ev.numRows(current)
          def numCols: Int = ev.numCols(current)
          def getRows(): ListBuffer[Vector[N]] = ev.getRows(current)
          def getColumns(): ListBuffer[Vector[N]] = ev.getColumns(current)
          def getRow(rowIndex: Int): Vector[N] = ev.getRow(current, rowIndex)
          def getColumn(colIndex: Int): Vector[N] = ev.getColumn(current, colIndex)
          def get(rowIndex: Int, colIndex: Int): N = ev.get(current, rowIndex, colIndex)
          def set(rowIndex: Int, colIndex: Int)(value: N): Unit = ev.set(current, rowIndex, colIndex, value)
     }

}

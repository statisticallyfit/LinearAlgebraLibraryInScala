package linalg.util

import linalg._
import linalg.implicits._
import linalg.vector.{SetOfVectors, Vector}
import cats.Eq
import linalg.matrix.{AugmentedMatrix, Matrix}

import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, Seq}
import scala.util.control.Breaks.{break, breakable}

/**
  *
  */
trait SetVecOps {


     /** Instances
       *
       * @return
       */

     def dimension[N: Number](vset: SetOfVectors[N]): Int = vset.getColumns().head.dimension()

     def eqv[N: Number](vset: SetOfVectors[N], wset: SetOfVectors[N]): Boolean = {
          try {
               Util.ensureSize(vset, wset)
          } catch {
               case _: Exception => false
          }

          //else if no exception ...
          vset.getColumns()
               .zip(wset.getColumns())
               .forall(colPair => Eq[Vector[N]].eqv(colPair._1, colPair._2))
     }

     def plus[N: Number](vset: SetOfVectors[N], wset: SetOfVectors[N]): SetOfVectors[N] ={
          Util.ensureSize(vset, wset)
          SetOfVectors(vset.getColumns().zip(wset.getColumns())
               .map(colPair => colPair._1 + colPair._2):_*)
     }

     def negate[N:Number](vset: SetOfVectors[N]): SetOfVectors[N] =
          SetOfVectors(vset.getColumns().map(c => c.negate()):_*)

     def scale[N:Number](vset: SetOfVectors[N], factor: N): SetOfVectors[N] =
          SetOfVectors(vset.getColumns().map(col => col.scale(factor)):_*)

     def isZero[N:Number](vset: SetOfVectors[N]): Boolean = vset.getColumns().forall(col => col.isZero)

     def identity[N:Number](size: Int): SetOfVectors[N] ={
          val list = ListBuffer.fill[N](size, size)(Number[N].zero)

          for(r <- 0 until size) {
               for(c <- 0 until size)
                    if(r == c)
                         list(r)(c) = Number[N].one
          }
          SetOfVectors.fromSeqs[N](list:_*)
     }


     //def rowEchelon[N: Number](vset: SetOfVectors[N]): SetOfVectors[N] = rref(vset, reduced = false)

     def rowReducedEchelon[N: Number](vset: SetOfVectors[N]): SetOfVectors[N] = {
          var echelonMatrix: SetOfVectors[N] = vset.copy()
          var lead: Int = 0
          val nRows: Int = vset.numRows
          val nCols: Int = vset.numCols

          breakable {
               for(r <- 0 until nRows){
                    if(lead >= nCols){
                         break
                    }
                    var i: Int = r
                    while (echelonMatrix.get(i, lead).isZero) { //then find the pivot element
                         i = i + 1
                         if (i == nRows){
                              i = r
                              lead = lead + 1
                              if(lead == nCols) { //then we have found last pivot
                                   return echelonMatrix
                              }
                         }
                    }

                    //swap rows i and r
                    if(i != r) echelonMatrix = swapRows(i, r, echelonMatrix)

                    //divide row r by rref[r][lead]
                    echelonMatrix = scaleRow(r, echelonMatrix.get(r, lead).inverse(), echelonMatrix)

                    for(j <- 0 until nRows){ //back-substitute upwards
                         if(j != r){  //subtract row r * -rref[j][lead] from row j
                              echelonMatrix = sumRows(j, r, echelonMatrix.get(j, lead).negate(), echelonMatrix)
                         }
                    }

                    lead = lead + 1 //now looking for a pivot further to the right
               }
          }
          //TODO nonzero rows alert fix where applicable SetOfVectors(expressRowsAsCols(getNonZeroRows(theRRef)):_*)
          SetOfVectors(Util.expressRowsAsCols(getNonZeroRows(echelonMatrix)):_*)
     }



     def span[N: Number](vset: SetOfVectors[N]): SetOfVectors[N] = rowReducedEchelon(vset)

     def doesSetSpanTheSpace[N: Number](vset: SetOfVectors[N]): Boolean ={
          val system = AugmentedMatrix(vset, Matrix.IDENTITY[N](vset))
          system.isConsistent()
     }

     def doesSetSpanTheVector[N: Number](vset: SetOfVectors[N], v: Vector[N]): Boolean ={
          val system = AugmentedMatrix(vset, v)
          system.isConsistent()
     }

     //Page 246 of howard: basis for the space spanned by a set of vectors
     def basis[N: Number](vset: SetOfVectors[N]): SetOfVectors[N] = {
          val freeCols: Seq[Int] = getIndicesOfFreeColumns(vset.rowReducedEchelon())
          SetOfVectors(vset.getColumnsAt(freeCols:_*):_*)
     }

     def isBasisOfSpace[N: Number](vset: SetOfVectors[N]): Boolean = vset === basis(vset)


     def getSpanningCoefficients[N: Number](vset: SetOfVectors[N], v: Vector[N]): Option[Matrix[N]] ={
          AugmentedMatrix(vset, v).solve()
     }

     def linearlyIndependent[N: Number](vset: SetOfVectors[N], wset: SetOfVectors[N]): Boolean ={
          val largestDim = Seq(vset.dimension(), wset.dimension()).max
          val aug = AugmentedMatrix(vset + wset, SetOfVectors.ZERO[N](largestDim))
          aug.rrefA === Matrix.IDENTITY[N](aug.rrefA)
     }

     def isLinearlyIndependent[N: Number](vset: SetOfVectors[N]): Boolean = {
          vset.rowReducedEchelon() === SetOfVectors.IDENTITY(vset)
     }

     def rank[N: Number](vset: SetOfVectors[N]): Int = {
          val rref: SetOfVectors[N] = Util.rowReducedEchelon(vset)
          rref.getRows().count(row => ! row.isZero)
     }





     // Utils ------------------------------------------------------------------------------------

     def colCombine[N:Number](vset: SetOfVectors[N], wset: SetOfVectors[N]): SetOfVectors[N] =
          SetOfVectors((vset.getColumns() ++ wset.getColumns()):_*)



     def ensureSize[N:Number](vset: SetOfVectors[N], wset: SetOfVectors[N]): Unit = {

          if(vset.numRows != wset.numRows || vset.numCols != wset.numCols) {
               throw Util.VectorLikeSizeException("SetOfVectors are not same size; cannot continue operation.")
          }
     }

     /**
       * Scales a row of a matrix.
       *
       * This method multiplies every value in a given row by a scale factor (scale).
       *
       * @param row   The row to be scaled
       * @param factor The scale factor (amount to be multiplied by)
       * @return The new Matrix with the row scaled
       */
     def scaleRow[N:Number](row: Int, factor: N, vset: SetOfVectors[N]): SetOfVectors[N] = {

          val rowMat: Seq[N] = vset.getRows().reduceLeft((accRow, yRow) =>
               Util.lengthCombine(accRow, yRow)).getElements()

          for (i <- row * vset.numCols until ((row + 1) * vset.numCols)) {
               rowMat(i) = rowMat(i) * factor
          }

          //converting from row to col representation
          val rows: Seq[Vector[N]] = Seq(rowMat.grouped(vset.numCols).toList.map(_.toVec):_*)

          SetOfVectors(expressRowsAsCols[N](rows): _*)
     }

     /**
       * @param rowA  The row to be added to.
       * @param rowB  The row to add.
       * @param scale The scale factor.
       * @return A new matrix, with the columns changed appropriately.
       */
     def sumRows[N:Number](rowA: Int, rowB: Int, scale: N, vset: SetOfVectors[N]): SetOfVectors[N] = {
          val oldMatList: Seq[N] = vset.getRows().reduceLeft((accRow, yRow) =>
               Util.lengthCombine(accRow, yRow)).getElements()
          val newMatList: Seq[N] = oldMatList
          for (i <- 0 until vset.numCols) { // for each value in rowA
               newMatList(rowA * vset.numCols + i) += oldMatList(rowB * vset.numCols + i) * scale //
          }

          // using grouped so we get cols again
          val rows: Seq[Vector[N]] = Seq(newMatList.grouped(vset.numCols).toList.map(_.toVec):_*)

          SetOfVectors(expressRowsAsCols[N](rows): _*)
     }

     /**
       * @param colA  The row to be added to.
       * @param colB  The row to add.
       * @param scale The scale factor.
       * @return A new matrix, with the columns changed appropriately.
       */
     def sumCols[N:Number](colA: Int, colB: Int, scale: N, vset: SetOfVectors[N]): SetOfVectors[N] = {
          val oldMatList: Seq[N] = vset.getColumns().reduceLeft((accCol, yCol) =>
               Util.lengthCombine(accCol, yCol)).getElements()
          val newMatList: Seq[N] = oldMatList
          for (i <- 0 until vset.numRows) { // for each value in rowA
               newMatList(colA * vset.numRows + i) += oldMatList(colB * vset.numRows + i) * scale //
          }

          SetOfVectors.fromSingleSeq[N](vset.numRows, vset.numCols, newMatList)
     }

     /**
       * Swaps two rows of a matrix.
       *
       * @param rowA First Row to be swapped
       * @param rowB Second Row to be swapped
       * @return A matrix with rows A and B swapped.
       */
     def swapRows[N:Number](rowA: Int, rowB: Int, vset: SetOfVectors[N]): SetOfVectors[N] = {
          val rows: Seq[Vector[N]] = vset.getRows()
          //swapping
          val temp: Vector[N] = rows(rowA)
          rows(rowA) = rows(rowB)
          rows(rowB) = temp

          SetOfVectors(expressRowsAsCols[N](rows): _*)
     }

     def swapCols[N:Number](colA: Int, colB: Int, vset: SetOfVectors[N]): SetOfVectors[N] = {
          val cols: Seq[Vector[N]] = vset.getColumns()
          //swapping
          val temp: Vector[N] = cols(colA)
          cols(colA) = cols(colB)
          cols(colB) = temp

          SetOfVectors(cols: _*)
     }

     /*def seqToVecSet[N:Number](seq: N*): SetOfVectors[N] = {
          val vset = SetOfVectors[N](seq.length, 0)
          for (i <- 0 until seq.length) vset.set(i, 0)(seq(i))
          vset
     }*/



     //precondition: expects the rref to come from undetermined system -- used for Solver
     // Gets indices of columns of original matrix with leading ones when in rref form.
     def getIndicesOfFreeColumns[N:Number](rref: SetOfVectors[N]): Array[Int] = {
          def countNonZero(v: Vector[N]): Int = v.getElements().count(e => ! e.isZero)

          //assumes we only have a single element in the vector
          def itsSingleElemIsNotOne(v: Vector[N]): Boolean = countNonZero(v) == 1 &&
               v.getElements().exists(e => e =!= Number[N].one &&  ! e.isZero)

          val vecIndexPair: Seq[(Vector[N], Int)] = rref.getColumns().zipWithIndex

          val indices = vecIndexPair.map({
               case (v, i) =>
                    if (countNonZero(v) > 1 || itsSingleElemIsNotOne(v))
                         Some(i)
                    else
                         None
          })
          //need options to preserve type else Array[Any]
          val indices2 = indices.filter(option => option.isDefined).map(_.get).toArray
          indices2
     }

     def expressRowsAsCols[N:Number](mat: SetOfVectors[N]): Seq[Vector[N]] =
          expressRowsAsCols(mat.getRows())

     def expressColsAsRows[N:Number](mat: SetOfVectors[N]): Seq[Vector[N]] =
          expressColsAsRows(mat.getColumns())

     def expressRowsAsCols[N:Number](rows: Seq[Vector[N]]): Seq[Vector[N]] = {
          //converting from row to col representation
          val ncol: Int = rows.head.dimension()
          val nrow: Int = rows.length
          val colBuff: Seq[Seq[N]] = Seq.fill[N](ncol, nrow)(Number[N].zero)

          for (c <- 0 until ncol) {
               colBuff(c) = rows.map(row => row.get(c))
          }
          colBuff.map(_.toVec)
     }

     def expressColsAsRows[N:Number](cols: Seq[Vector[N]]): Seq[Vector[N]] = {
          expressRowsAsCols(cols)
     }


     def getNonZeroRows[N:Number](vset: SetOfVectors[N]): Seq[Vector[N]] =
          vset.getRows().filterNot(row => row.isZero)


}

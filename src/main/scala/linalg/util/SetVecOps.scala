package linalg.util

import linalg._
import linalg.implicits._
import linalg.vector.{SetOfVectors, Vector}
import cats.Eq
import linalg.kernel.Rational
import linalg.matrix.{AugmentedMatrix, Matrix, SquareMatrix}

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

     def size[N: Number](vset: SetOfVectors[N]): (Int, Int) = (vset.numRows, vset.numCols)

     def dimension[N: Number](vset: SetOfVectors[N]): Int = Ops.basisOfSpaceSpannedBySet(vset).numCols

     def eqv[N: Number](vset: SetOfVectors[N], wset: SetOfVectors[N]): Boolean = {
          vset.size() == wset.size() match {
               case false => false
               case true =>{
                    //else if no exception ...
                    vset.getColumns()
                         .zip(wset.getColumns())
                         .forall(colPair => Eq[Vector[N]].eqv(colPair._1, colPair._2))
               }
          }
     }

     def plus[N: Number](vset: SetOfVectors[N], wset: SetOfVectors[N]): SetOfVectors[N] ={
          Ops.ensureSize(vset, wset)
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


     def rowEchelon[N: Number](vset: SetOfVectors[N]): SetOfVectors[N] =
          rowEchelonNoFractions(vset)

     private def rowEchelonNoFractions[N: Number](vset: SetOfVectors[N]): SetOfVectors[N] ={
          var echelon: SetOfVectors[N] = rref(vset, reduced = false)

          for(r <- 0 until echelon.numRows){
               val denoms: Seq[Int] = echelon.getRow(r).getElements().map(e => e.denominator())
               val lcm: N = Number[N].from(Ops.lcmSeq(denoms:_*))
               echelon = scaleRow(r, lcm, echelon)
          }
          echelon
     }

     def rowReducedEchelon[N: Number](vset: SetOfVectors[N]): SetOfVectors[N] = {
          rref(vset, reduced=true)
     }

     private def rref[N: Number](vset: SetOfVectors[N], reduced: Boolean): SetOfVectors[N] ={
          var echelon: SetOfVectors[N] = vset.copy(); // Clone Matrix so algorithm may be performed in place.

          var i: Int = 0
          var j: Int = 0

          while(i < vset.numRows && j < vset.numCols) {
               //look for target value in column
               var iMax: Int = i

               for (k <- (i + 1) until vset.numRows) {
                    if (echelon.get(k, j) > echelon.get(iMax, j)) {
                         iMax = k
                    }
               }
               if (!echelon.get(iMax, j).isZero) {
                    // Swap pivot row into place
                    echelon = swapRows(i, iMax, echelon)
                    // Scale pivot row
                    echelon = scaleRow(i, echelon.get(i, j).inverse(), echelon)
                    // Fill all lower rows with 0
                    for (u <- (i + 1) until vset.numRows) {
                         echelon = sumRows(u, i, echelon.get(u, j).negate(), echelon)
                    }
                    if (reduced) { //if above cols need filling ...
                         val reversedIndices = (0 to (i - 1)).reverse
                         for (u <- reversedIndices) {
                              echelon = sumRows(u, i, echelon.get(u, j).negate(), echelon)
                         }
                    }
                    // next row
                    i = i + 1
               }
               j = j + 1 //next column
          }
          return echelon //Return the transformed matrix.
     }


     // Span

     def span[N: Number](vset: SetOfVectors[N]): SetOfVectors[N] = Ops.rowReducedEchelon(vset)

     def isSpanningSpace[N: Number](vset: SetOfVectors[N], dim: Int): Boolean ={
          //or page 206 rref == identity method
          //val rref = vset.reducedEchelon()

          //or exercises 4.3 david lay method:
          /*val rref = vset.reducedEchelon()
          getIndicesOfLeadingColumns(rref).length == dim*/

          if(vset.numRows != dim) return false
          //else
          val system = AugmentedMatrix(vset, SquareMatrix.IDENTITY[N](dim))
          system.isConsistent()
     }

     def isSpanningVector[N: Number](vset: SetOfVectors[N], v: Vector[N]): Boolean ={
          val system = AugmentedMatrix(vset, v)
          system.isConsistent()
     }

     def getSpanningCoefficients[N: Number](vset: SetOfVectors[N], v: Vector[N]): Option[Matrix[N]] ={
          Ops.solve(AugmentedMatrix(vset, v))
     }

     // Basis

     //Page 246 of howard: basis for the space spanned by a set of vectors
     def basisOfSpaceSpannedBySet[N: Number](vset: SetOfVectors[N]): SetOfVectors[N] = {
          val ref = Ops.rowReducedEchelon(vset)
          val leadingCols: Seq[Int] = getIndicesOfLeadingColumns(ref)
          SetOfVectors(vset.getColumnsAt(leadingCols:_*):_*)
     }

     def alternateBasisOfSpaceSpannedBySet[N: Number](vset: SetOfVectors[N]): SetOfVectors[N] = {
          val nonzeroColVecs = Ops.getNonZeroRows(Ops.rowReducedEchelon(Ops.transpose(vset)))
          SetOfVectors(nonzeroColVecs:_*)
     }

     // example 3.52 in david lay: checks if the given set is a basis
     def isBasisOfSet[N: Number](vset: SetOfVectors[N], maybeBasis: SetOfVectors[N]): Boolean ={
          val basis1: SetOfVectors[N] = Ops.basisOfSpaceSpannedBySet(vset)
          val basis2: SetOfVectors[N] = Ops.alternateBasisOfSpaceSpannedBySet(vset)


          if (maybeBasis.size() != basis1.size())  {
               return false
          }
          val rref1: SetOfVectors[N] = Ops.basisOfSpaceSpannedBySet(maybeBasis) //reduce further to find true
          // character
          val rref2: SetOfVectors[N] = Ops.alternateBasisOfSpaceSpannedBySet(maybeBasis) //reduce further to find true
          // character

          (rref1 === basis1 || rref1 === basis2) || (rref2 === basis1 || rref2 === basis2)
     }

     //no need to make this - just need to have below method isBasisOfSpace() , given some vecs, show they are a
     // basis for the relevant space.
     // no need to make this since by proposition 2.27 singh, any n x n lin indep vec set is a basis for R^n. (so many)
     /*def basisOfSpace[N: Number](vset: SetOfVectors[N]): SetOfVectors[N] = {
     }*/


     // Checks if the rref form is the identity matrix (see exercises 4.3 in David Lay)
     //from page 206 of david poole (fundamental theorem of invertible matrices)
     // dim = dimension of space we want to check if the set is a basis of.
     def isBasisOfSpace[N: Number](vset: SetOfVectors[N], dim: Int): Boolean = {
          Ops.Id.isSquare(vset.toMatrix) &&
               vset.numRows == dim &&
               Ops.rowReducedEchelon(vset) === SetOfVectors.IDENTITY(vset)
     }


     // linear independence
     def linearlyIndependent[N: Number](vset: SetOfVectors[N], wset: SetOfVectors[N]): Boolean ={
          val entire: SetOfVectors[N] = Ops.colCombine(vset, wset)
          val rref: SetOfVectors[N] = Ops.rowReducedEchelon(entire)
          rref === SetOfVectors.IDENTITY(rref) //TODO
          /*vset.size() == wset.size() match {
               case false => false
               case true => {
                    //TODO is it correct to use the numCols?
                    val dim = Seq(vset.dimension(), wset.dimension()).max
                    val aug = AugmentedMatrix(vset + wset, SetOfVectors.ZERO[N](dim))
                    aug.rrefA === Matrix.IDENTITY[N](aug.rrefA)
               }
          }*/
     }

     def isLinearlyIndependent[N: Number](vset: SetOfVectors[N]): Boolean = {
          val rref = Ops.rowReducedEchelon(vset)
          rref === SetOfVectors.IDENTITY(rref)
     }

     // row space

     def rank[N: Number](vset: SetOfVectors[N]): Int = {
          //val rref: SetOfVectors[N] = Util.rowReducedEchelon(vset)
          //rref.getRows().count(row => ! row.isZero)
          rowSpace(vset).dimension()
     }

     def isFullRank[N: Number](vset: SetOfVectors[N]): Boolean = Ops.rank(vset) == vset.numRows


     def isInRowSpace[N: Number](vset: SetOfVectors[N], v: Vector[N]): Boolean ={
          isInColumnSpace(Ops.transpose(vset), v)
     }

     def equalRowSpaces[N: Number](vset1: SetOfVectors[N], vset2: SetOfVectors[N]): Boolean = {
          Ops.rowReducedEchelon(vset1) === Ops.rowReducedEchelon(vset2)
     }

     def rowSpace[N: Number](vset: SetOfVectors[N]): SetOfVectors[N] = {
          SetOfVectors(Ops.expressRowsAsCols(Ops.getNonZeroRows(Ops.rowReducedEchelon(vset))):_*)
     }
     //TODO
     def areRowsSpanningSpace[N: Number](vset: SetOfVectors[N]): Boolean ={
          Ops.rowReducedEchelon(vset) === SetOfVectors.IDENTITY(vset)
     }

     // column space
     def isInColumnSpace[N: Number](vset: SetOfVectors[N], v: Vector[N]): Boolean = {
          AugmentedMatrix(vset, v).isConsistent()
     }

     //TODO
     def equalColSpaces[N: Number](vset1: SetOfVectors[N], vset2: SetOfVectors[N]): Boolean ={
          columnSpace(vset1) === columnSpace(vset2)
     }

     def columnSpace[N: Number](vset: SetOfVectors[N]): SetOfVectors[N] ={
          val leadingCols: Seq[Int] = getIndicesOfLeadingColumns(Ops.rowReducedEchelon(vset))
          SetOfVectors(vset.getColumnsAt(leadingCols:_*):_*)
     }

     //TODO
     def areColsSpanningSpace[N: Number](vset: SetOfVectors[N]): Boolean = ???

     //TODO
     def columnRank[N: Number](vset: SetOfVectors[N]): Int = columnSpace(vset).dimension()


     // Null space
     def isInNullSpace[N: Number](vset: SetOfVectors[N], v: Vector[N]): Boolean = {
           vset.toMatrix * v.toMatrix === Matrix.ZERO[N](vset.numRows, 1) //vector matrix
     }

     //TODO
     def equalNullSpaces[N: Number](vset1: SetOfVectors[N], vset2: SetOfVectors[N]): Boolean = {
          nullSpace(vset1) === nullSpace(vset2)
     }

     def nullSpace[N: Number](vset: SetOfVectors[N]): SetOfVectors[N] = {
          AugmentedMatrix(vset, Vector.ZERO[N](vset.numRows)).solve().get
          //TODO is it possible for this to not be defined?
     }

     //TODO def areColsSpanningSpace(): Boolean = ev.areColsSpanningSpace(current)
     //TODO def areColsBasisOfSpace(): Boolean = ev.areColsBasisOfSpace(current)

     def nullity[N: Number](vset: SetOfVectors[N]): Int = {
          nullSpace(vset).dimension()
     }






     // Utils ------------------------------------------------------------------------------------

     def colCombine[N:Number](vset: SetOfVectors[N], wset: SetOfVectors[N]): SetOfVectors[N] = {
          SetOfVectors((vset.getColumns() ++ wset.getColumns()):_*)
          /*if(vset.numRows != wset.numRows){
               throw Util.MatrixLikeSizeException("vset.numRows must equal wset.numRows")
          } else {
               SetOfVectors((vset.getColumns() ++ wset.getColumns()):_*)
          }*/
     }


     def ensureSize[N:Number](vset: SetOfVectors[N], wset: SetOfVectors[N]): Unit = {

          if(vset.numRows != wset.numRows || vset.numCols != wset.numCols) {
               throw Ops.MatrixLikeSizeException("SetOfVectors are not same size; cannot continue operation.")
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
               Ops.lengthCombine(accRow, yRow)).getElements()

          for (i <- row * vset.numCols until ((row + 1) * vset.numCols)) {
               rowMat(i) = rowMat(i) * factor
          }

          //converting from row to col representation
          val rows: Seq[Vector[N]] = Seq(rowMat.grouped(vset.numCols).toList.map(_.toVec):_*)

          SetOfVectors(Ops.expressRowsAsCols[N](rows): _*)
     }

     /**
       * @param rowA  The row to be added to.
       * @param rowB  The row to add.
       * @param scale The scale factor.
       * @return A new matrix, with the columns changed appropriately.
       */
     def sumRows[N:Number](rowA: Int, rowB: Int, scale: N, vset: SetOfVectors[N]): SetOfVectors[N] = {
          val oldMatList: Seq[N] = vset.getRows().reduceLeft((accRow, yRow) =>
               Ops.lengthCombine(accRow, yRow)).getElements()
          val newMatList: Seq[N] = oldMatList
          for (i <- 0 until vset.numCols) { // for each value in rowA
               newMatList(rowA * vset.numCols + i) += oldMatList(rowB * vset.numCols + i) * scale //
          }

          // using grouped so we get cols again
          val rows: Seq[Vector[N]] = Seq(newMatList.grouped(vset.numCols).toList.map(_.toVec):_*)

          SetOfVectors(Ops.expressRowsAsCols[N](rows): _*)
     }

     /**
       * @param colA  The row to be added to.
       * @param colB  The row to add.
       * @param scale The scale factor.
       * @return A new matrix, with the columns changed appropriately.
       */
     def sumCols[N:Number](colA: Int, colB: Int, scale: N, vset: SetOfVectors[N]): SetOfVectors[N] = {
          val oldMatList: Seq[N] = vset.getColumns().reduceLeft((accCol, yCol) =>
               Ops.lengthCombine(accCol, yCol)).getElements()
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

          SetOfVectors(Ops.expressRowsAsCols[N](rows): _*)
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



     def transpose[N: Number](vset: SetOfVectors[N]): SetOfVectors[N] = SetOfVectors(vset.getRows(): _*)



     // Gets indices of columns of original matrix with leading ones when in rref form.
     def getIndicesOfLeadingColumns[N: Number](rref: SetOfVectors[N]): Array[Int] ={
          val indices = 0 until rref.numCols
          (indices diff Ops.getIndicesOfFreeColumns(rref)).toArray
     }

     //precondition: expects the rref to come from undetermined system -- used for Solver
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
          val indices2 = indices.filter(option => option.isDefined).map(_.get)
          indices2.toArray
     }

     def expressRowsAsCols[N:Number](mat: SetOfVectors[N]): Seq[Vector[N]] =
          Ops.expressRowsAsCols(mat.getRows())

     def expressColsAsRows[N:Number](mat: SetOfVectors[N]): Seq[Vector[N]] =
          Ops.expressColsAsRows(mat.getColumns())

     def expressRowsAsCols[N:Number](rows: Seq[Vector[N]]): Seq[Vector[N]] = {
          //converting from row to col representation
          val ncol: Int = rows.head.size()
          val nrow: Int = rows.length
          val colBuff: Seq[Seq[N]] = Seq.fill[N](ncol, nrow)(Number[N].zero)

          for (c <- 0 until ncol) {
               colBuff(c) = rows.map(row => row.get(c))
          }
          colBuff.map(_.toVec)
     }

     def expressColsAsRows[N:Number](cols: Seq[Vector[N]]): Seq[Vector[N]] = {
          Ops.expressRowsAsCols(cols)
     }


     def getNonZeroRows[N:Number](vset: SetOfVectors[N]): Seq[Vector[N]] =
          vset.getRows().filterNot(row => row.isZero)


}

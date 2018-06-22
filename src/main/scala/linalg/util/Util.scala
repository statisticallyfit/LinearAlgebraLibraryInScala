package linalg.util


import linalg.implicits._
import linalg._
import linalg.matrix.{Matrix, SquareMatrix}
import linalg.vector.{SetOfVectors, Vector}

import scala.collection.mutable.{ListBuffer, Seq}
import scala.language.higherKinds
import scala.language.implicitConversions
import scala.util.control.Breaks.{break, breakable}

/**
  *
  */

object Util {


     object Exception {

          /**
            * Thrown when matrix or vectors are not same size for addition etc.
            */
          case class VectorLikeSizeException(message: String) extends Exception(message)
     }

     object Gen {

          /**
            * Inserts element at position i, leaving list the same length as before.
            */
          def insert[N: Number](elem: N, i: Int, list: List[N]): List[N] = {
               val (first, second) = list.splitAt(i)
               (first :+ elem) ++: second.tail
          }

          /**
            * Inserts list at position i, leaving list of lists the same length as before.
            */
          def insert[N: Number](elems: List[N], i: Int, list: List[List[N]]): List[List[N]] = {
               val (first, second) = list.splitAt(i)
               (first :+ elems) ++: second.tail
          }

          def roundTo(double: Double, places: Int): Double = {
               //step 1: multiply by the correct number of zeroes = numplaces
               val factor: Double = ("1" + List.fill[Int](places)(0).mkString).toDouble
               (double * factor).round / factor
          }

          def getNonZeroRows[N:Number](vset: SetOfVectors[N]): Seq[Vector[N]] =
               vset.getRows().filterNot(row => row.isZero)

          def lengthCombine[N:Number](v: Vector[N], w: Vector[N]): Vector[N] =
               Vector((v.getElements() ++ w.getElements()):_*)

          def colCombine[N:Number](v: Vector[N], w: Vector[N]): SetOfVectors[N] =
               SetOfVectors(v, w)

          def colCombine[N:Number](vset: SetOfVectors[N],
                                   wset: SetOfVectors[N]): SetOfVectors[N] =
               SetOfVectors((vset.getColumns() ++ wset.getColumns()):_*)

          def sumElements[N:Number](v: Vector[N]): N = v.getElements().reduceLeft[N](_ + _)


          def ensureSize[N:Number](v: Vector[N], w: Vector[N], SIZE: Int = 0): Unit = {

               SIZE match {
                    case 0 => if(v.dimension() != w.dimension()) {
                         throw Exception.VectorLikeSizeException("Vectors are not same size; cannot continue operation.")
                    }
                    case _ => {
                         val len = v.dimension()

                         if (v.dimension() == w.dimension() && SIZE != len){
                              throw Exception.VectorLikeSizeException("Vectors do not have same size as given size; cannot " +
                                   "continue operation.")

                         } else if (v.dimension() != w.dimension()){
                              throw Exception.VectorLikeSizeException("Vectors do not have same size; cannot continue operation.")
                         }
                    }
               }
          }

          def ensureSize[N:Number](vset: SetOfVectors[N], wset: SetOfVectors[N]): Unit = {

               if(vset.numRows != wset.numRows || vset.numCols != wset.numCols) {
                    throw Exception.VectorLikeSizeException("SetOfVectors are not same size; cannot continue operation.")
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

               val rowMat: Seq[N] = vset.getRows().reduceLeft((accRow, yRow) => lengthCombine(accRow, yRow)).getElements()

               for (i <- row * vset.numCols until ((row + 1) * vset.numCols)) {
                    rowMat(i) = rowMat(i) * factor
               }

               //converting from row to col representation
               val rows: Seq[Vector[N]] = Seq(rowMat.toList.grouped(vset.numCols).toList
                    .map(list => Vector(list: _*)): _*)

               SetOfVectors(expressRowsAsCols[N](rows): _*)
          }

          /**
            * @param rowA  The row to be added to.
            * @param rowB  The row to add.
            * @param scale The scale factor.
            * @return A new matrix, with the columns changed appropriately.
            */
          def sumRows[N:Number](rowA: Int, rowB: Int, scale: N, vset: SetOfVectors[N]): SetOfVectors[N] = {
               val oldMatList: Seq[N] = vset.getRows().reduceLeft((accRow, yRow) => lengthCombine(accRow, yRow)).getElements()
               val newMatList: Seq[N] = oldMatList
               for (i <- 0 until vset.numCols) { // for each value in rowA
                    newMatList(rowA * vset.numCols + i) += oldMatList(rowB * vset.numCols + i) * scale //
               }

               // using grouped so we get cols again
               val rows: Seq[Vector[N]] = Seq(newMatList.grouped(vset.numCols).toList
                    .map(list => new Vector(list: _*)): _*)

               SetOfVectors(expressRowsAsCols[N](rows): _*)
          }

          /**
            * @param colA  The row to be added to.
            * @param colB  The row to add.
            * @param scale The scale factor.
            * @return A new matrix, with the columns changed appropriately.
            */
          def sumCols[N:Number](colA: Int, colB: Int, scale: N, vset: SetOfVectors[N]): SetOfVectors[N] = {
               val oldMatList: Seq[N] = vset.getColumns().reduceLeft((accCol, yCol) => lengthCombine(accCol, yCol)).getElements()
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

          def seqToVecSet[N:Number](seq: N*): SetOfVectors[N] = {
               val vset = SetOfVectors[N](seq.length, 0)
               for (i <- 0 until seq.length) vset.set(i, 0)(seq(i))
               vset
          }

          def seqToVec[N: Number](seq: Seq[N]): Vector[N] = Vector(seq: _*)


          //precondition: expects the rref to come from undetermined system -- used for Solver
          def getIndicesOfFreeColumns[N:Number](rref: SetOfVectors[N]): Array[Int] = {
               def countNonZero(v: Vector[N]): Int = v.getElements().count(e => e != 0)

               //assumes we only have a single element in the vector
               def itsSingleElemIsNotOne(v: Vector[N]): Boolean = countNonZero(v) == 1 &&
                    v.getElements().exists(e => e != 1 && e != 0)

               val vecIndexPair: Seq[(Vector[N], Int)] = rref.getColumns().zipWithIndex

               val indices = vecIndexPair.map({
                    case (v, i) =>
                         if (countNonZero(v) > 1 || itsSingleElemIsNotOne(v))
                              Some(i)
                         else
                              None
               })
               //need options to preserve type else Array[Any]
               val indices2 = indices.filter(op => op.isDefined).map(_.get).toArray
               indices2
          }

          def expressRowsAsCols[N:Number](rows: Seq[Vector[N]]): Seq[Vector[N]] = {
               //converting from row to col representation
               val ncol: Int = rows.head.dimension()
               val nrow: Int = rows.length
               val colBuff: Seq[Seq[N]] = Seq.fill[N](ncol, nrow)(Number.ZERO[N])

               for (c <- 0 until ncol) {
                    colBuff(c) = rows.map(row => row.get(c))
               }
               colBuff.map(buff => new Vector(buff: _*))
          }

          def expressColsAsRows[N:Number](cols: Seq[Vector[N]]): Seq[Vector[N]] = {
               expressRowsAsCols(cols)
          }


          def rowEchelon[N: Number](vset: SetOfVectors[N]): SetOfVectors[N] = rref(vset, reduced = false)

          def rowReducedEchelon[N: Number](vset: SetOfVectors[N]): SetOfVectors[N] = {
               val theRRef: SetOfVectors[N] = rref(vset, reduced=true)
               SetOfVectors(Util.Gen.expressRowsAsCols(Util.Gen.getNonZeroRows(theRRef)):_*)
          }

          private def rref[N: Number](vset: SetOfVectors[N], reduced: Boolean): SetOfVectors[N] ={
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
                         if(i != r) echelonMatrix = Util.Gen.swapRows(i, r, echelonMatrix)

                         //divide row r by rref[r][lead]
                         echelonMatrix = Util.Gen.scaleRow(r, echelonMatrix.get(r, lead).inverse(), echelonMatrix)

                         for(j <- 0 until nRows){ //back-substitute upwards
                              if(j != r){  //subtract row r * -rref[j][lead] from row j
                                   echelonMatrix = Util.Gen.sumRows(j, r,
                                        echelonMatrix.get(j, lead).negate(),
                                        echelonMatrix)
                              }
                         }

                         lead = lead + 1 //now looking for a pivot further to the right
                    }
               }

               echelonMatrix
          }
     }




     // ------------------------------------------------------------------------------

     object MatrixOps {

          //TODO - where to put this? in each class or should I use typeclasses?
          //TODO but then would have to make squarematlike typeclass -- too grainy.
          object Id {

               def isSymmetric[N: Number](smat: SquareMatrix[N]): Boolean={
                    smat == smat.transpose()
               }

               def isHermitian[N: Number](smat: SquareMatrix[N]): Boolean={
                    smat == smat.conjugateTranspose()
               }

               //TODO need shapeless to automatically derive typeclass instances for squaremat??
               def isUnitary[N: Number](smat: SquareMatrix[N]): Boolean ={
                    SquareMatrix.IDENTITY[N](smat.dimension()) == (smat * smat.conjugateTranspose())
               }

               def isOrthogonal[N: Number](smat: SquareMatrix[N]): Boolean ={
                    SquareMatrix.IDENTITY[N](smat.dimension()) == (smat * smat.transpose())
               }

               def isLowerTriangular[N: Number](mat: Matrix[N]): Boolean ={
                    for(r <- 0 until mat.numRows){
                         for(c <- 0 until mat.numCols){
                              if(c > r && mat.get(r, c) != 0)
                                   false
                         }
                    }
                    true
               }

               def isUpperTriangular[N: Number](mat: Matrix[N]): Boolean ={
                    for(r <- 0 until mat.numRows){
                         for(c <- 0 until mat.numCols){
                              if(c < r && mat.get(r, c) != 0)
                                   false
                         }
                    }
                    true
               }
          }

          object Trans {
               object HessenbergTransformer {
                    import linalg.implicits._
                    /**
                      * Computes the upper Hessenberg form of the matrix via similarity transforms
                      * based on Gauss-Jordan elimination
                      */
                    def makeHessenberg[N: Number](smat: SquareMatrix[N])
                                                 (implicit a: Absolute[N, N]): SquareMatrix[N] = {

                         var squareHessenMat: SetOfVectors[N] = smat.asInstanceOf[SetOfVectors[N]].copy()

                         for(r <- 0 until squareHessenMat.numRows){
                              //find max magnitude in the rth col below diagonal
                              var largest: Double = 0
                              var largestRow:Int = 0

                              for(i <- (r+1) until squareHessenMat.numRows){
                                   //TODO test here if the .abs() still works when at runtime
                                   // TODO when using Complex class -- need to differentiate
                                   // TODO between layerops and regular?
                                   if(squareHessenMat.get(i, r).abs().toDouble > largest){
                                        largest = squareHessenMat.get(i, r).abs().toDouble
                                        largestRow = i
                                   }
                              }

                              if(largest != 0){
                                   //interchange rows largestRow and r+1
                                   squareHessenMat = Util.Gen.swapRows(largestRow, r+1, squareHessenMat)
                                   //interchange cols largestRow and r+1 to make it a similarity transform
                                   squareHessenMat = Util.Gen.swapCols(largestRow, r+1, squareHessenMat)

                                   for(i <- (r+2) until squareHessenMat.numRows){
                                        val mult: N = squareHessenMat.get(i, r) / squareHessenMat.get(r+1, r)

                                        //subtract mult * row r+1 from row i
                                        squareHessenMat = Util.Gen.sumRows(i, r+1, mult, squareHessenMat)
                                        //add mult * col i to col r+1 to preserve similarity
                                        squareHessenMat = Util.Gen.sumCols(r+1, i, mult, squareHessenMat)
                                   }
                              }
                         }
                         squareHessenMat.asInstanceOf[SquareMatrix[N]]
                    }
               }
          }
     }
}



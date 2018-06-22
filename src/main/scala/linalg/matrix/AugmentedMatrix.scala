package linalg.matrix

/**
  *
  */
class AugmentedMatrix {

}


//class AugmentedMatrix[N <: Number[N]: TypeTag](A: Matrix[N], B: Matrix[N])
//     extends Matrix[N](A.combine(B).getColumns():_*) {
//
//     private val rrefThis: Matrix[N] = this.reducedRowEchelon() //note implicit vecset => matrix
//     private val rrefA: Matrix[N] = A.reducedRowEchelon() //note implicit vecset => matrix
//     private val rrefB: Matrix[N] = Matrix(rrefThis.getColumns().takeRight(B.numCols):_*)
//
//     //todo make apply methods for these
//     //note using explicit method since using implicit would cause constructor error
//     def this(A: Matrix[N], b: Vector[N]) = this(A, Matrix(b))
//     def this(A: Matrix[N]) = this(A, Matrix.ZERO[N](A.numRows, 1))
//
//
//     def getA(): Matrix[N] = A
//     def getB(): Matrix[N] = B
//
//     def isConsistent(): Boolean = !isInconsistent()
//     def isInconsistent(): Boolean = rrefThis.getRows()
//          .exists(row =>
//               row.toBuffer.init.forall(_.isZero) &&
//                    row.toBuffer.drop(A.numCols).exists(e => !e.isZero))
//
//     def hasNoSolution(): Boolean = isInconsistent()
//     def hasUniqueSolution(): Boolean = rrefA == Matrix.IDENTITY(rrefA)
//     def hasInfiniteSolutions(): Boolean = rrefA != Matrix.IDENTITY(rrefA)
//
//     private def infiniteSolutionSolver(): Matrix[N] ={
//          //get the indices of free cols
//          val freeIndices: Array[Int] = Util.GenOps.getIndicesOfFreeColumns(rrefA)
//          //get the freecolumns and attach to each column another zero vector to make it length = numcolsrref
//          val freeCols: List[Vector[N]] = rrefA.getColumns().zipWithIndex
//               .filter(colIndexPair => freeIndices.contains(colIndexPair._2)).map(_._1).toList
//
//          //STEP 1: make the matrix of free cols
//          var free: VectorSet[N] = new Matrix[N](freeCols:_*)
//          //remove any zero rows
//          free = Matrix(free.getRows().filterNot(vec => vec.isZero):_*).transpose() //transpose so rows again
//          //STEP 2: minus the B rows with nonzero free rows (Brows - freerows), this can just be multiplied by -1
//          // since if hwe have just free variable cols then the constants will not minus these.
//          free = free.scale(-1)
//          //new Matrix(B.getRows().take(free.numRows).zip(free.getRows()).map(p => p._1 - p._2):_*).transpose()
//
//          // make new "matrix" from listbuffer that is old rref transposed with numcol = numfree and zeroes
//          // everywhere but in row positions where free col positions we put rows of identity matrix
//          val id: ListBuffer[ListBuffer[N]] = Matrix.IDENTITY[N](free.numCols).getRows().map(_.toBuffer)
//          val sol: ListBuffer[ListBuffer[N]] = ListBuffer.fill[N](rrefA.numCols, free.numCols)(Number.ZERO[N])
//          val freeRows: ListBuffer[ListBuffer[N]] = free.getRows().map(_.toBuffer)
//          //fill the row pos with identity rows corresponding to free col pos
//          var freeRowIndex: Int = 0
//          var idRowIndex: Int = 0
//          var r:Int = 0
//          while(r < sol.length) {
//               if(freeIndices.contains(r)) {
//                    breakable {
//                         if(idRowIndex >= id.length) break
//                         //else
//                         sol(r) = id(idRowIndex)
//                         idRowIndex = idRowIndex + 1
//                    }
//               } else {
//                    breakable {
//                         if(freeRowIndex >= freeRows.length) break
//                         //else, it's a zero row and then put the free col rows in it
//                         sol(r) = freeRows(freeRowIndex)
//                         freeRowIndex = freeRowIndex + 1
//                    }
//               }
//               r = r + 1
//          }
//
//          val solution: Matrix[N] = Matrix.fromBuffers(sol:_*).transpose()
//
//          //then add the B cols to the front of our solution, never the case for kernel where B={0}
//          if(B.isZero())
//               solution
//          else {
//               //else making constants column and elongating it such that it is as long as solution numrows
//               // Step 1: first get the pivot indexes (where pivot 1)
//               val indices: Array[Int] = (0 until rrefA.numCols).toArray
//               val pivotIndices: Array[Int] = indices.diff(freeIndices)
//               // Step 2: zip pivotindices with rrefB - assert always will be same length since
//               // it's sliced from rrefThis. Filter to get tuples with nonzero rrefB elements.
//               val tuples = pivotIndices.zip(Util.GenOps.expressColsAsRows(rrefB.toListOfLists))
//               val tuplesNoZeroRows = tuples.filter({
//                    case (index, elems) => !elems.forall(_ == Number.ZERO[N])
//               })
//               // Step 3: fill zeroes between the elements indices.
//               val maxIndex: Int = tuplesNoZeroRows.map(_._1).max // get max index to make list
//               var newRrefB: List[List[N]] = Util.GenOps.expressColsAsRows(
//                    Matrix.ZERO[N](maxIndex + 1, rrefB.numCols).toListOfLists)
//               // inserting the elements in the tuples at the indices.
//               for((index, elems) <- tuplesNoZeroRows){
//                    newRrefB = Util.GenOps.insert(elems, index, newRrefB)
//               }
//               newRrefB = Util.GenOps.expressRowsAsCols(newRrefB)
//               Matrix.fromLists(newRrefB:_*).combine(solution) //note implicit vecset => matrix
//          }
//     }
//
//
//     def solve(): Option[Matrix[N]] ={
//          if(hasNoSolution()) None
//          else if(hasUniqueSolution()) Some(Matrix[N](rrefThis.getColumns().takeRight(B.numCols):_*))
//          else Some(infiniteSolutionSolver())
//     }
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
//object AugmentedMatrix {
//     def apply[N <: Number[N]: TypeTag](A: Matrix[N], B: Matrix[N]): AugmentedMatrix[N] ={
//          new AugmentedMatrix[N](A, B)
//     }
//
//     def apply[N <: Number[N]: TypeTag](A: Matrix[N], b: Vector[N]): AugmentedMatrix[N] ={
//          new AugmentedMatrix[N](A, b)
//     }
//}
//
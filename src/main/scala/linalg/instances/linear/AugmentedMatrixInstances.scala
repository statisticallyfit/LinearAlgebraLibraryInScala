package linalg.instances.linear

import spire.algebra.Eq
import linalg.implicits._
import linalg._
import linalg.matrix.{AugmentedMatrix, Matrix, SquareMatrix}
import linalg.vector.{SetOfVectors, Vector}
import linalg.util._

import scala.collection.mutable.{ListBuffer, Seq}
import scala.language.higherKinds
import scala.language.implicitConversions
import scala.util.control.Breaks._
/**
  *
  */

class AugmentedMatrixThings[N: Number] {

     /*class MatrixHasAbsoluteValue extends Absolute[Matrix[N], Matrix[N]] {

          def absoluteValue(mat: Matrix[N]): Matrix[N] =
               Matrix(mat.getColumns().map(vec => vec.abs()):_*)
     }*/

     class AugmentedMatrixHasDimension extends Dimension[AugmentedMatrix[N]]{
          def dimension(mat: AugmentedMatrix[N]): Int = Util.dimension(mat)
     }

     class AugmentedMatrixHasEq extends Eq[AugmentedMatrix[N]]{
          def eqv(mat1: AugmentedMatrix[N], mat2: AugmentedMatrix[N]): Boolean = Util.eqv(mat1, mat2)
     }

     class AugmentedMatrixIsMonoid extends Monoid[AugmentedMatrix[N]]{

          val zero: AugmentedMatrix[N] = AugmentedMatrix(Vector.ZERO[N](1))
          def plus(mat1: AugmentedMatrix[N], mat2: AugmentedMatrix[N]): AugmentedMatrix[N] =
               Util.plus(mat1, mat2).toAugMatrix
     }

     class AugmentedMatrixIsAbelianGroup extends AugmentedMatrixIsMonoid with AbelianGroup[AugmentedMatrix[N]]{
          def negate(mat: AugmentedMatrix[N]): AugmentedMatrix[N] = Util.negate(mat).toAugMatrix
     }

     class AugmentedMatrixIsVectorSpace extends AugmentedMatrixIsAbelianGroup with VectorSpace[AugmentedMatrix[N], N]{
          val one: AugmentedMatrix[N] = AugmentedMatrix(Vector.ONE[N](1))
          def scale(mat: AugmentedMatrix[N], factor: N): AugmentedMatrix[N] = Util.scale(mat, factor).toAugMatrix
     }

     class AugmentedMatrixIsSetVecLike extends AugmentedMatrixIsVectorSpace with SetVecLike[AugmentedMatrix[N], N]{
          def isZero(mat: AugmentedMatrix[N]): Boolean = Util.isZero(mat)
          def identity(size: Int): AugmentedMatrix[N] = AugmentedMatrix(Util.identity(size).getColumns():_*)
          def rowEchelon(mat: AugmentedMatrix[N]): AugmentedMatrix[N] =
               Util.rowEchelon(mat).toAugMatrix
          def rowReducedEchelon(mat: AugmentedMatrix[N]): AugmentedMatrix[N] =
               Util.rowReducedEchelon(mat).toAugMatrix

     }

     class AugmentedMatrixIsMatrixLike extends AugmentedMatrixIsSetVecLike with MatrixLike[AugmentedMatrix[N], N] {

          def power(mat: AugmentedMatrix[N], exp: N): AugmentedMatrix[N] = Util.power(mat, exp).toAugMatrix
          def inverse(mat: AugmentedMatrix[N]): AugmentedMatrix[N] = Util.inverse(mat).toAugMatrix
          def transpose(mat: AugmentedMatrix[N]): AugmentedMatrix[N] = Util.transpose(mat).toAugMatrix
          def conjugateTranspose(mat: AugmentedMatrix[N]): AugmentedMatrix[N] = Util.conjugateTranspose(mat).toAugMatrix
          def adjoint(mat: AugmentedMatrix[N]): AugmentedMatrix[N] =  Util.adjoint(mat).toAugMatrix
          def cofactor(mat: AugmentedMatrix[N]): AugmentedMatrix[N] = Util.cofactor(mat).toAugMatrix
          def cofactor(mat: AugmentedMatrix[N], r:Int, c:Int): N = Util.cofactor(mat, r, c)
          def minor(mat: AugmentedMatrix[N]): AugmentedMatrix[N] = Util.minor(mat).toAugMatrix
          def minor(mat: AugmentedMatrix[N], r: Int, c: Int): N = Util.minor(mat, r, c)
          def determinant(mat: AugmentedMatrix[N]): N = Util.determinant(mat)
          def trace(mat: AugmentedMatrix[N]): N = Util.trace(mat)
     }


     class AugmentedMatrixIsLinearSystem extends AugmentedMatrixIsMatrixLike with LinearSystem[AugmentedMatrix[N], N]{


          def isInconsistent(mat: AugmentedMatrix[N]): Boolean = {
               //does there exist a zero row in rrefA where the same row has consts in the rrefB?
               mat.rrefAll.getRows().exists(row => row.getElements().init.forall(_.isZero) &&
                    row.getElements().drop(mat.A.numCols).exists(elem => ! elem.isZero))
          }

          //Assume: the system of equations is in matrix A and the user has passed the correct
          // identity matrix as matrix B. //TODO correct? Or just skip B and make the correct identity mat here???
          def hasUniqueSolution(mat: AugmentedMatrix[N]): Boolean =
               mat.rrefA === Matrix.IDENTITY[N](mat.rrefA)


          def hasInfiniteSolutions(mat: AugmentedMatrix[N]): Boolean = {
               isConsistent(mat) && mat.rrefA === Matrix.IDENTITY[N](mat.rrefA)
          }

          def infiniteSolutionSolver(mat: AugmentedMatrix[N]): AugmentedMatrix[N] ={
               //get the indices of free cols
               val freeIndices: Array[Int] = Util.getIndicesOfFreeColumns(mat.rrefA)
               //get the freecolumns and attach to each column another zero vector to make it length = numcolsrref
               val freeCols: List[Vector[N]] = mat.rrefA.getColumns().zipWithIndex
                    .filter(colIndexPair => freeIndices.contains(colIndexPair._2)).map(_._1).toList

               //STEP 1: make the matrix of free cols
               var free: SetOfVectors[N] = new Matrix[N](freeCols:_*)
               //remove any zero rows
               free = Matrix(free.getRows().filterNot(vec => vec.isZero):_*).transpose() //transpose so rows again
               //STEP 2: minus the B rows with nonzero free rows (Brows - freerows), this can just be multiplied by -1
               // since if hwe have just free variable cols then the constants will not minus these.
               free = free.scale(Number[N].one.negate())
               //new Matrix(B.getRows().take(free.numRows).zip(free.getRows()).map(p => p._1 - p._2):_*).transpose()

               // make new "matrix" from listbuffer that is old rref transposed with numcol = numfree and zeroes
               // everywhere but in row positions where free col positions we put rows of identity matrix
               val id: Seq[Seq[N]] = Matrix.IDENTITY[N](free.numCols).getRows().map(vec => vec.toListB)
               val sol: Seq[Seq[N]] = Seq.fill[N](mat.rrefA.numCols, free.numCols)(Number.ZERO[N])
               val freeRows: Seq[Seq[N]] = free.getRows().map(vec => vec.toListB)
               //fill the row pos with identity rows corresponding to free col pos
               var freeRowIndex: Int = 0
               var idRowIndex: Int = 0
               var r:Int = 0
               while(r < sol.length) {
                    if(freeIndices.contains(r)) {
                         breakable {
                              if(idRowIndex >= id.length) break
                              //else
                              sol(r) = id(idRowIndex)
                              idRowIndex = idRowIndex + 1
                         }
                    } else {
                         breakable {
                              if(freeRowIndex >= freeRows.length) break
                              //else, it's a zero row and then put the free col rows in it
                              sol(r) = freeRows(freeRowIndex)
                              freeRowIndex = freeRowIndex + 1
                         }
                    }
                    r = r + 1
               }

               val solution: Matrix[N] = Matrix.fromSeqs(sol:_*).transpose()

               //then add the B cols to the front of our solution, never the case for kernel where B={0}
               if(mat.B.isZero)
                    solution.toAugMatrix
               else {
                    //else making constants column and elongating it such that it is as long as solution numrows
                    // Step 1: first get the pivot indexes (where pivot 1)
                    val indices: Array[Int] = (0 until mat.rrefA.numCols).toArray
                    val pivotIndices: Array[Int] = indices.diff(freeIndices)
                    // Step 2: zip pivotindices with rrefB - assert always will be same length since
                    // it's sliced from rrefThis. Filter to get tuples with nonzero rrefB elements.
                    val tuples = pivotIndices.zip(Util.expressColsAsRows(mat.rrefB))
                    val tuplesNoZeroRows = tuples.filter({
                         case (index, vec) => vec.getElements().forall(e => e.isZero)
                    })
                    // Step 3: fill zeroes between the elements indices.
                    val maxIndex: Int = tuplesNoZeroRows.map(_._1).max // get max index to make list
                    var newRrefB: Seq[Vector[N]] = Util.expressColsAsRows(
                         Matrix.ZERO[N](maxIndex + 1, mat.rrefB.numCols))

                    // inserting the elements in the tuples at the indices.
                    for((index, elems) <- tuplesNoZeroRows){
                         newRrefB = Util.insert(elems, index, newRrefB)
                    }
                    newRrefB = Util.expressRowsAsCols(newRrefB)

                    AugmentedMatrix(Matrix(newRrefB:_*), solution)
               }
          }


          def solve(mat: AugmentedMatrix[N]): Option[Matrix[N]] ={
               if(hasNoSolution(mat)) None
               else if(hasUniqueSolution(mat)) Some(Matrix[N](mat.rrefAll.getColumns().takeRight(mat.B.numCols):_*))
               else Some(infiniteSolutionSolver(mat))
          }
     }

     //span, basis ... etc


     //val absolute = new MatrixHasAbsoluteValue
     val eq = new AugmentedMatrixHasEq
     val dim = new AugmentedMatrixHasDimension
     val monoid = new AugmentedMatrixIsMonoid
     val abelian = new AugmentedMatrixIsAbelianGroup
     val vectorSpace = new AugmentedMatrixIsVectorSpace
     val setVecLike = new AugmentedMatrixIsSetVecLike
     val matrixLike = new AugmentedMatrixIsMatrixLike
     val linearSystem = new AugmentedMatrixIsLinearSystem
}


trait AugmentedMatrixInstances {

     //implicit final def matrixHasAbsoluteValue[N: Number] = new MatrixThings[N].absolute
     implicit final def augMatrixIsMonoid[N: Number] = new AugmentedMatrixThings[N].monoid
     implicit final def augMatrixIsAbelianGroup[N: Number] = new AugmentedMatrixThings[N].abelian
     implicit final def augMatrixIsVectorSpace[N: Number] = new AugmentedMatrixThings[N].vectorSpace
     implicit final def augMatrixIsSetVecLike[N: Number] = new AugmentedMatrixThings[N].setVecLike
     implicit final def augMatrixHasEq[N: Number] = new AugmentedMatrixThings[N].eq
     implicit final def augMatrixHasDimension[N: Number] = new AugmentedMatrixThings[N].dim
     implicit final def augMatrixIsMatrixLike[N: Number] = new AugmentedMatrixThings[N].matrixLike
     implicit final def augMatrixIsLinearSystem[N: Number] = new AugmentedMatrixThings[N].linearSystem

}


package linalg.util

import linalg._
import linalg.implicits._
import linalg.matrix.{AugmentedMatrix, Matrix, SquareMatrix}
import linalg.vector.{SetOfVectors, Vector}

import scala.collection.mutable.{ListBuffer, Seq}
import scala.util.control.Breaks.{break, breakable}

/**
  *
  */
trait MatrixOps {

     def multiply[N: Number](mat1: Matrix[N], mat2: Matrix[N]): Matrix[N] = {
          Ops.ensureSizeMat(mat1, mat2)

          val prod: Seq[Seq[N]] = Seq.fill[N](mat1.numCols, mat1.numRows)(Number[N].zero)
          for(r <- 0 until mat1.numRows){
               for(c <- 0 until mat2.numCols){
                    for(k <- 0 until mat1.numCols){
                         //note it would be get(r, k) and get(k, c) if our internal matrices were represented with
                         // rows, but they are represented in cols so use this inverted call.
                         prod(c)(r) = (prod(c)(r) + mat1.get(k, r)) * mat2.get(c, k)
                    }
               }
          }
          Matrix.fromSeqs[N](prod:_*)
     }
     def power[N: Number](mat1: Matrix[N], exp: N): Matrix[N] = ???

     def inverse[N: Number](mat: Matrix[N]): Matrix[N] = ??? // TODO - solve using augmented?

     def conjugateTranspose[N: Number](mat: Matrix[N]): Matrix[N] =
          Ops.transpose(Matrix.fromSeqs(mat.getColumns().map(col => col.getElements()
               .map(e => e.conjugate())):_*)).toMatrix

     def adjoint[N: Number](mat: Matrix[N]): Matrix[N] = {
          //definition 6.19: adjoint = transpose(cofactor) if matrix is square
          //can apply this definition only if matrix is square, not checking this here.
          val newMat: ListBuffer[Vector[N]] = ListBuffer() //ListBuffer.fill[N](numRows, numCols)(Number.ZERO[N])

          for(r <- 0 until mat.numRows){
               val row: Vector[N] = Vector.ZERO[N](mat.numCols)

               for(c <- 0 until mat.numCols){
                    val cof = ((r + c) % 2 == 0) match {
                         case true => minor(mat, r, c)
                         case false => minor(mat, r, c).negate()
                    }
                    row.set(c)(cof)
               }
               newMat += row
          }
          //implicit transpose (we could have done (matrix (rowtocol(mat)).transpose
          Matrix(newMat:_*)
     }

     def cofactor[N: Number](mat: Matrix[N]): Matrix[N] ={

          val indexes: IndexedSeq[(Int, Int)] = for(r <- 0 until mat.numRows; c <- 0 until mat.numCols) yield (r, c)
          val cofactors = indexes.map(indexPair => cofactor(mat, indexPair._1, indexPair._2))
          Ops.transpose(Matrix.fromFlat(mat.numRows, mat.numCols, cofactors))
     }

     def cofactor[N: Number](mat: Matrix[N], r:Int, c:Int): N = ((r + c) % 2 == 0) match {
          case true => minor(mat, r, c)
          case false => minor(mat, r, c).negate()
     }

     def minor[N: Number](mat: Matrix[N]): Matrix[N] ={
          val indexes: IndexedSeq[(Int, Int)] = for(r <- 0 until mat.numRows; c <- 0 until mat.numCols) yield (r, c)
          val minors = indexes.map(indexPair => minor(mat, indexPair._1, indexPair._2))
          Ops.transpose(Matrix.fromFlat(mat.numRows, mat.numCols, minors)) //isrow=true since we travel along
          // rows.
     }

     def minor[N: Number](mat: Matrix[N], r: Int, c: Int): N ={
          //note: new matrix has numRows - 1 columns
          //note: row v has length numCols - 1
          val newMat: ListBuffer[Vector[N]] = ListBuffer()
          for(k1 <- 0 until mat.numRows){
               if(k1 != r){
                    val row: Vector[N] = mat.getRow(k1) //mat.getRows()(k1)
                    val v: Vector[N] = Vector.ZERO[N](mat.numCols - 1)
                    newMat += v

                    var k: Int = 0
                    for(k2 <- 0 until mat.numCols){
                         if(k2 != c) {
                              v.set(k)(row.get(k2))
                              k = k + 1
                         }
                    }
               }
          }
          Ops.determinant(Ops.transpose(Matrix.fromSeq(newMat)))
     }

     def determinant[N: Number](mat: Matrix[N]): N ={
          //proposition 6.12: if square then A * adj(A) = det(A), proof page 449
          //note but don't actually implementat that way since adjoint depends on determinant
          //todo what to do if not square? implement?
          ??? //new LUFactorization[N](this).determinant()
     }

     def trace[N: Number](mat: Matrix[N]): N = {
          val diag: Vector[N] = Vector.ZERO[N](mat.numCols) //note: go by numcols since never an extra left

          for(c <- 0 until mat.numCols){
               for (r <- 0 until mat.numRows){
                    if(r == c){
                         diag.set(c)(mat.get(r, c))
                    }
               }
          }
          Ops.sumElements(diag)
     }




     //does there exist a zero row in rrefA where the same row has consts in the rrefB?
     def isInconsistent[N: Number](mat: AugmentedMatrix[N]): Boolean = {
          mat.rrefEntire.getRows().exists(row =>
               row.getElements().take(mat.rrefA.numCols).forall(elem => elem.isZero) &&
               row.getElements().drop(mat.rrefA.numCols).exists(elem => ! elem.isZero))
     }

     //Assume: the system of equations is in matrix A and the user has passed the correct
     // identity matrix as matrix B. //TODO correct? Or just skip B and make the correct identity mat here???
     def hasUniqueSolution[N: Number](mat: AugmentedMatrix[N]): Boolean =
          mat.rrefA === Matrix.IDENTITY[N](mat.rrefA)


     def hasInfiniteSolutions[N: Number](mat: AugmentedMatrix[N]): Boolean = {
          mat.isConsistent() && mat.rrefA === Matrix.IDENTITY[N](mat.rrefA)
     }

     def infiniteSolutionSolver[N: Number](mat: AugmentedMatrix[N]): Matrix[N] ={
          //get the indices of free cols
          val freeIndices: Array[Int] = Ops.getIndicesOfFreeColumns(mat.rrefA)
          //get the freecolumns and attach to each column another zero vector to make it length = numcolsrref
          val freeCols: List[Vector[N]] = mat.rrefA.getColumns().zipWithIndex
               .filter(colIndexPair => freeIndices.contains(colIndexPair._2)).map(_._1).toList

          //STEP 1: make the matrix of free cols
          var free: Matrix[N] = Matrix[N](freeCols:_*)
          //remove any zero rows
          free = Matrix(free.getRows().filterNot(vec => vec.isZero):_*).transpose() //transpose so rows again
          //STEP 2: minus the B rows with nonzero free rows (Brows - freerows), this can just be multiplied by -1
          // since if hwe have just free variable cols then the constants will not minus these.
          free = free.scale(Number[N].one.negate())
          //new Matrix(B.getRows().take(free.numRows).zip(free.getRows()).map(p => p._1 - p._2):_*).transpose()

          // make new "matrix" from listbuffer that is old rref transposed with numcol = numfree and zeroes
          // everywhere but in row positions where free col positions we put rows of identity matrix
          val id: Seq[Seq[N]] = Matrix.IDENTITY[N](free.numCols).getRows().map(_.getElements())
          val sol: Seq[Seq[N]] = Seq.fill[N](mat.rrefA.numCols, free.numCols)(Number.ZERO[N])
          val freeRows: Seq[Seq[N]] = free.getRows().map(_.getElements())
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


          //TODO separate this part to be getParticularSolution() and getGeneralSolution()
          //TODO from page 240 of howard
          //then add the B cols to the front of our solution, never the case for kernel where B={0}
          if(mat.B.isZero)
               solution
          else {
               //else making constants column and elongating it such that it is as long as solution numrows
               // Step 1: first get the pivot indexes (where pivot 1)
               val indices: Array[Int] = (0 until mat.rrefA.numCols).toArray
               val pivotIndices: Array[Int] = indices.diff(freeIndices)
               // Step 2: zip pivotindices with rrefB - assert always will be same length since
               // it's sliced from rrefThis. Filter to get tuples with nonzero rrefB elements.
               val tuples = pivotIndices.zip(Ops.expressColsAsRows(mat.rrefB))
               val tuplesNoZeroRows = tuples.filter({
                    case (index, vec) => ! vec.getElements().forall(e => e.isZero)
               })
               // Step 3: fill zeroes between the elements indices.
               val maxIndex: Int = tuplesNoZeroRows.map(_._1).max // get max index to make list

               var newRrefB: Seq[Vector[N]] =
                    Ops.expressColsAsRows(Matrix.ZERO[N](maxIndex + 1,
                         mat.rrefB.numCols))

               // inserting the elements in the tuples at the indices.
               for((index, vec) <- tuplesNoZeroRows){
                    newRrefB = Ops.insertVec(vec, index, newRrefB)
               }
               newRrefB = Ops.expressRowsAsCols(newRrefB)

               Ops.colCombine(Matrix(newRrefB:_*), solution).toMatrix
          }
     }

     def solve[N: Number](mat: AugmentedMatrix[N]): Option[Matrix[N]] ={
          if(mat.hasNoSolution()) None
          else if(hasUniqueSolution(mat)) Some(Matrix[N](mat.rrefEntire.getColumns().takeRight(mat.B.numCols):_*))
          else Some(infiniteSolutionSolver(mat))
     }












     def ensureSizeMat[N:Number](mat1: Matrix[N], mat2: Matrix[N]): Unit = {

          if(mat1.numCols != mat2.numRows) {
               throw Ops.MatrixLikeSizeException("Matrices do not have correct size for multiplication.")
          }
     }











     //TODO - where to put this? in each class or should I use typeclasses?
     //TODO but then would have to make squarematlike typeclass -- too grainy.
     object Id {

          def isIdentity[N: Number](mat: Matrix[N]): Boolean = {
               if(!isSquare(mat)) return false

               for(r <- 0 until mat.numRows){
                    for(c <- 0 until mat.numCols){
                         if((r != c && !mat.get(r,c).isZero) || (r == c && !mat.get(r,c).isZero)){
                              return false //found a violation
                         }
                    }
               }
               true // no violation
          }

          def isSquare[N: Number](mat: Matrix[N]): Boolean = mat.numRows == mat.numCols

          def isSymmetric[N: Number](mat: Matrix[N]): Boolean = mat === mat.transpose()

          def isSkewSymmetric[N: Number](mat: Matrix[N]): Boolean = mat === mat.transpose().negate()

          def isHermitian[N: Number](mat: Matrix[N]): Boolean = mat === mat.conjugateTranspose()

          def isUnitary[N: Number](mat: Matrix[N]): Boolean = isIdentity(mat * mat.conjugateTranspose())

          def isOrthogonal[N: Number](mat: Matrix[N]): Boolean = isIdentity(mat * mat.transpose())

          def isLowerTriangular[N: Number](mat: Matrix[N]): Boolean ={
               if(!isSquare(mat)) return false

               for(r <- 0 until mat.numRows){
                    for(c <- 0 until mat.numCols){
                         if(c > r && mat.get(r, c) != 0)
                              return false
                    }
               }
               true
          }

          def isUpperTriangular[N: Number](mat: Matrix[N]): Boolean ={
               if(!isSquare(mat)) return false

               for(r <- 0 until mat.numRows){
                    for(c <- 0 until mat.numCols){
                         if(c < r && mat.get(r, c) != 0)
                              return false
                    }
               }
               true
          }

          //TODO add more identity matrix functions for all kinds: Hessian, hilbert ... etc
          def isHilbert[N: Number](mat: Matrix[N]): Boolean ={
               if(!isSquare(mat)) return false

               for(r <- 0 until mat.numRows) {
                    for(c <- 0 until mat.numCols) {

                         val hilbertValue: N = Number[N].one / Number[N].from(r + c - 1)

                         if(mat.get(r,c) =!= hilbertValue){
                              return false
                         }
                    }
               }
               true
          }
          //TODO if want to keep hessenberg, see this definition:
          //TODO https://people.cs.kuleuven.be/~raf.vandebril/homepage/publications/papers_html/imp_q_ss/node2.html
     }

     object Trans {
          object HessenbergTransformer {
               import linalg.implicits._
               /**
                 * Computes the upper Hessenberg form of the matrix via similarity transforms
                 * based on Gauss-Jordan elimination
                 */
               def makeHessenberg[N: Number](smat: SquareMatrix[N])
               /*(implicit a: Absolute[N, N])*/: SquareMatrix[N] = {

                    /*var squareHessenMat: SetOfVectors[N] = smat.asInstanceOf[SetOfVectors[N]].copy()

                    for(r <- 0 until squareHessenMat.numRows){
                         //find max magnitude in the rth col below diagonal
                         var largest: N = Number[N].zero
                         var largestRow:Int = 0

                         for(i <- (r+1) until squareHessenMat.numRows){
                              //TODO test here if the .abs() still works when at runtime
                              // TODO when using Complex class -- need to differentiate
                              // TODO between layerops and regular?

                              //todo abs doesn't work on number just on real since only
                              // real inherits from Absolute[R, R] but number cannot.

                              if(squareHessenMat.get(i, r).abs() > largest){
                                   largest = squareHessenMat.get(i, r).abs()
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
                    squareHessenMat.asInstanceOf[SquareMatrix[N]]*/
                    ???

                    //TODO why doesn't abs value work? -- because Number doesn't inherit
                    // Absolute trait
                    //TODO hoping this will work when implementing asbolute for SquareMat
               }
          }
     }
}

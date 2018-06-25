package linalg.util

import linalg._
import linalg.implicits._
import linalg.matrix.{Matrix, SquareMatrix}
import linalg.vector.Vector

import scala.collection.mutable.ListBuffer

/**
  *
  */
trait MatrixOps {


     def power[N: Number](mat1: Matrix[N], exp: N): Matrix[N] = ???

     def inverse[N: Number](mat: Matrix[N]): Matrix[N] = ??? // TODO - solve using augmented?

     def transpose[N: Number](mat: Matrix[N]): Matrix[N] = Matrix(mat.getRows(): _*)

     def conjugateTranspose[N: Number](mat: Matrix[N]): Matrix[N] =
          transpose(Matrix.fromSeqs(mat.getColumns().map(col => col.getElements()
               .map(e => e.conjugate())):_*))

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
          val cofactors: Seq[N] = indexes.map(indexPair => cofactor(mat, indexPair._1, indexPair._2))
          transpose(Matrix.fromSeq(mat.numRows, mat.numCols, cofactors))
     }

     def cofactor[N: Number](mat: Matrix[N], r:Int, c:Int): N = ((r + c) % 2 == 0) match {
          case true => minor(mat, r, c)
          case false => minor(mat, r, c).negate()
     }

     def minor[N: Number](mat: Matrix[N]): Matrix[N] ={
          val indexes: IndexedSeq[(Int, Int)] = for(r <- 0 until mat.numRows; c <- 0 until mat.numCols) yield (r, c)
          val minors: Seq[N] = indexes.map(indexPair => minor(mat, indexPair._1, indexPair._2))
          transpose(Matrix.fromSeq(mat.numRows, mat.numCols, minors)) //isrow=true since we travel along rows.
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
          determinant(transpose(Matrix(newMat:_*)))
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
          Util.sumElements(diag)
     }








     //TODO - where to put this? in each class or should I use typeclasses?
     //TODO but then would have to make squarematlike typeclass -- too grainy.
     object Id {

          def isSquare[N: Number](mat: Matrix[N]): Boolean = mat.numRows == mat.numCols

          def isSymmetric[N: Number](smat: SquareMatrix[N]): Boolean={
               ??? //smat == smat.transpose()
          }

          def isHermitian[N: Number](smat: SquareMatrix[N]): Boolean={
               ??? //smat == smat.conjugateTranspose()
          }

          //TODO need shapeless to automatically derive typeclass instances for squaremat??
          def isUnitary[N: Number](smat: SquareMatrix[N]): Boolean ={
               ???
               ///SquareMatrix.IDENTITY[N](smat.dimension()) == (smat * smat.conjugateTranspose())
          }

          def isOrthogonal[N: Number](smat: SquareMatrix[N]): Boolean ={
               ???
               //SquareMatrix.IDENTITY[N](smat.dimension()) == (smat * smat.transpose())
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

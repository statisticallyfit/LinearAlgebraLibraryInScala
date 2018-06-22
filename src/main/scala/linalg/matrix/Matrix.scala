package linalg.matrix

import linalg.implicits._
import linalg._
import linalg.vector.{SetOfVectors, Vector}
import org.apache.commons.lang3.StringUtils
import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.universe._
import scala.util.control.Breaks._



////todo have object methods: isIdentity() and isSquare() and so forth
////note: matrix types:
////Square, Hessenberg, LowerTri, UpperTri, Unitary,
////Orthogonal, Hermitian, Symmetric, Diagonal, Hilbert
//// Similar, UpperHessenberg, Jacobian.
//

//TODO get rid of 'extends' by using Hlist shapeless
class Matrix[N: Number](private val cols: Vector[N]*) extends SetOfVectors[N](cols:_*)

/*object Testing {
     def main(args: Array[String]) {
          val m = new Matrix[Int](Vector(1,2,3), Vector(3,4,5))
          m + m
          m.dimension()
     }
}*/
//
///*
//class Matrix[N <: Number[N]: TypeTag](cols: Vector[N]*) extends VectorSet[N](cols:_*) {
//
//
//     def this(mat: Matrix[N]) = this(mat.getColumns():_*)
//     def this(rowNum:Int, colNum:Int) = this(Matrix.ZERO[N](rowNum, colNum))
//
//
//     //key: doing divide and inverse without using option like above (so relying on getting left or right inverse
//     //todo specify later if it is right or left inverse
//     def /(that: Matrix[N]): Matrix[N] = this * that.inverse()
//
//     def inverse(): Matrix[N] = new AugmentedMatrix[N](this, Matrix.IDENTITY(this)).solve().get
//
//     def transpose(): Matrix[N] = new Matrix(getRows():_*)
//
//     def adjoint(): Matrix[N] = {
//          //definition 6.19: adjoint = transpose(cofactor) if matrix is square.
//          val mat: ListBuffer[Vector[N]] = ListBuffer() //ListBuffer.fill[N](numRows, numCols)(Number.ZERO[N])
//
//          for(r <- 0 until numRows){
//               val row: Vector[N] = Vector(numCols)
//               mat += row
//               for(c <- 0 until numCols){
//                    val cof = ((r + c) % 2 == 0) match {
//                         case true => this.minor(r, c)
//                         case false => this.minor(r, c).opposite()
//                    }
//                    row.set(c, cof)
//               }
//          }
//          Matrix(mat:_*) //implicit transpose (we could have done (matrix (rowtocol(mat)).transpose
//     }
//
//     def cofactor(): Matrix[N] ={
//          val indexes: IndexedSeq[(Int, Int)] = for(r <- 0 until numRows; c <- 0 until numCols) yield (r, c)
//          val cofactors: List[N] = indexes.map(indexPair => cofactor(indexPair._1, indexPair._2)).toList
//          Matrix.fromList(numRows, numCols, cofactors).transpose()
//     }
//
//     def cofactor(r:Int, c:Int): N = ((r + c) % 2 == 0) match {
//          case true => minor(r, c)
//          case false => minor(r, c).opposite()
//     }
//
//     def minor(): Matrix[N] ={
//          val indexes: IndexedSeq[(Int, Int)] = for(r <- 0 until numRows; c <- 0 until numCols) yield (r, c)
//          val minors: List[N] = indexes.map(indexPair => minor(indexPair._1, indexPair._2)).toList
//          Matrix.fromList(numRows, numCols, minors).transpose() //isrow=true since we travel along rows.
//     }
//
//     def minor(r: Int, c: Int): N ={
//          //note: new matrix has numRows - 1 columns
//          //note: row v has length numCols - 1
//          val mat: ListBuffer[Vector[N]] = ListBuffer()
//          for(k1 <- 0 until numRows){
//               if(k1 != r){
//                    val row: Vector[N] = this.getRows()(k1)
//                    val v: Vector[N] = Vector(numCols - 1)
//                    mat += v
//
//                    var k: Int = 0
//                    for(k2 <- 0 until numCols){
//                         if(k2 != c) {
//                              v.set(k, row.get(k2))
//                              k = k + 1
//                         }
//                    }
//               }
//          }
//          Matrix(mat:_*).transpose().determinant()
//     }
//
//     /*val det = complexMatrixJSCI.determinant()
//     Number.toNumber(Complex(det.getReal, det.getImaginary).asInstanceOf[N])*/
//     def determinant(): N = {
//          //proposition 6.12: if square then A * adj(A) = det(A), proof page 449
//          //note but don't actually implementat that way since adjoint depends on determinant
//          //todo what to do if not square? implement?
//          new LUFactorization[N](this).determinant()
//     }
//
//     def trace(): N = this.getDiagonal().elementSum
//
//     def conjugateTranspose(): Matrix[N] = {
//          Number.runtimeType[N] match {
//               case "Real" => this
//               case "Rational" => this
//               case "Complex" => Matrix.fromList(numCols, numRows, this.transpose().toList
//                    .map(e => e.asInstanceOf[Complex].conjugate().asInstanceOf[N]))
//          }
//     }
//
//     //def toVectorSet: VectorSet[N] = this.asInstanceOf[VectorSet[N]] // todo check if it works
//
//     //key find way to eliminate this (or do it more elegantly)
//     //key not using it makes val squaremat = matrix.copy complain since vecset is higher than squaremat
//    /* def toSquareMatrix: SquareMatrix[N] = {
//          if(numRows != numCols) throw new Exception("Not square -- cannot make it object of class square matrix")
//
//          new SquareMatrix(this)
//     }*/
//}
//
//
//
//
object Matrix {
     def apply[N: Number](nr: Int, nc: Int): Matrix[N] = Matrix.ZERO[N](nr, nc)

     def apply[N: Number](cols: Vector[N]*): Matrix[N] = new Matrix(cols: _*)


     def ZERO[N: Number](mat: Matrix[N]): Matrix[N] =
          Matrix.fromBuffers(ListBuffer.fill[N](mat.numCols, mat.numRows)(Number.ZERO[N]): _*)

     def ZERO[N: Number](nrows: Int, ncols: Int): Matrix[N] =
          Matrix.fromBuffers(ListBuffer.fill[N](ncols, nrows)(Number.ZERO[N]): _*)

     def IDENTITY[N: Number](size: Int): Matrix[N] = {
          val list = ListBuffer.fill[N](size, size)(Number.ZERO[N])
          for (r <- 0 until size) {
               for (c <- 0 until size) if (r == c) list(r)(c) = Number.ONE[N]
          }
          Matrix.fromBuffers(list.toList: _*)
     }

     //makes identity matrix using which is bigger - numrows or numcols
     def IDENTITY[N: Number](mat: Matrix[N]): Matrix[N] = {
          if (mat.numCols == mat.numCols && mat.numCols == 1) new Matrix[N](new Vector[N](Number.ONE[N]))
          val size = Math.max(mat.numCols, mat.numRows)
          val list = ListBuffer.fill[N](size, size)(Number.ZERO[N])
          for (r <- 0 until size) {
               for (c <- 0 until size) if (r == c) list(r)(c) = Number.ONE[N]
          }
          Matrix.fromBuffers(list.toList: _*)
     }

     def fromBuffers[N: Number](buffs: ListBuffer[N]*): Matrix[N] =
          new Matrix(buffs.map(list => new Vector(list: _*)): _*)

     def fromLists[N: Number](lists: List[N]*): Matrix[N] =
          new Matrix(lists.map(list => new Vector(list: _*)): _*)

     //help why do the matrix[rational] and matrix[complex] turn errorish matrix[nothing] when i name this apply?
     def fromList[N: Number](nr: Int, nc: Int, list: Seq[N] /*, isRow: Boolean*/): Matrix[N] = {
          //assume data is along column
          Matrix.fromLists(list.grouped(nc).toList.map(_.toList): _*)
     }
}


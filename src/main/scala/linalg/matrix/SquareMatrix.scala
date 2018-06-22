package linalg.matrix

import linalg._
import linalg.implicits._
import linalg.vector.{Vector, SetOfVectors}
import linalg.util._

import scala.collection.mutable.ListBuffer

/**
  *
  */
class SquareMatrix[N: Number](mat: Matrix[N]) extends Matrix[N](mat.getColumns():_*)



object SquareMatrix {

     def apply[N: Number](n:Int): SquareMatrix[N] =
          new SquareMatrix(Matrix.ZERO[N](n, n))

     def apply[N: Number](cols: Vector[N]*): SquareMatrix[N] = new SquareMatrix(Matrix(cols:_*))

     def ZERO[N: Number](n:Int): SquareMatrix[N] = new SquareMatrix(Matrix.ZERO[N](n, n))

     def IDENTITY[N: Number](size:Int): SquareMatrix[N] = new SquareMatrix(Matrix.IDENTITY[N](size))

     def DIAGONAL[N: Number](diag: Vector[N]): SquareMatrix[N] ={

          val mat: ListBuffer[ListBuffer[N]] = ListBuffer()
          for(r <- 0 until diag.dimension()){
               for(c <- 0 until diag.dimension()){
                    if(r == c) mat(r)(c) = diag.get(r)
                    else mat(r)(c) = Number.ZERO.asInstanceOf[N]
               }
          }
          new SquareMatrix(Matrix.fromBuffers(mat:_*))
     }

     def DIAGONAL[N: Number](elem: N, size: Int): SquareMatrix[N] ={
          DIAGONAL(Vector(List.fill[N](size)(elem):_*))
     }

}


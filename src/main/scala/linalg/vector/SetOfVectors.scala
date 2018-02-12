package linalg.vector

import linalg.numeric._
import linalg.numeric.Number._
import linalg.theory.space._
import linalg.theory.basis._
import linalg.syntax.DimensionSyntax._
import linalg.vector.VectorLike._
import linalg.syntax.VectorLikeSyntax._
import linalg.syntax.VectorSpaceSyntax._
import linalg.syntax.SetVecLikeSyntax._

import scala.collection.mutable.ListBuffer

import scala.language.higherKinds
import scala.language.implicitConversions

/**
  *
  */


trait SetVecLike[V, F] extends VectorSpace[V, F]{

     def identity(size: Int): V
     def rowReducedEchelon(m: V): V
     def minus(v: V, w: V): V = plus(v, negate(w))
     def isZero(v: V): Boolean
     def numRows(v: V): Int
     def numCols(v: V): Int

     def getRows(v: V): ListBuffer[Vector[F]]
     def getRow(v: V, rowIndex: Int): Vector[F] //todo ok? will polymorph e.g. polynomial?

     def getColumns(v: V): ListBuffer[Vector[F]]
     def getColumn(v: V, colIndex: Int): Vector[F]

     def get(v: V, rowIndex: Int, colIndex: Int)(implicit vec: VectorLike[Vector[F],F]): F =
          vec.get(getRow(v, rowIndex), colIndex) //todo why doesn't implicit work?

     def set(v: V, rowIndex: Int, colIndex: Int, value: F): Unit
     //def todo setRow and setCol?
}


object SetVecLike {

     implicit def VectorSetIsVectorSetLike[V[_], N: Number: Trig: Absolute: Root: Compare](implicit vecLike:
     VectorSpace[V[N], N]) = new SetVecLike[SetOfVectors[N], N] with Dimension[SetOfVectors[N]] {


          val zero: SetOfVectors[N] = SetOfVectors(Vector.ZERO[N](1))
          val one: SetOfVectors[N] = SetOfVectors(Vector.ONE[N](1))

          def plus(vset: SetOfVectors[N], w: SetOfVectors[N]): SetOfVectors[N] =
               SetOfVectors(vset.columns.zip(w.columns).map(colPair => colPair._1 + colPair._2):_*)

          def negate(vset: SetOfVectors[N]): SetOfVectors[N] = SetOfVectors(vset.columns.map(c => c.negate()):_*)

          def scale(v: SetOfVectors[N], factor: N): SetOfVectors[N] =
               SetOfVectors(v.columns.map(col => col.scale(factor)):_*)

          def isZero(v: SetOfVectors[N]): Boolean = v.columns.forall(col => col.isZero())


          def dimension(vset: SetOfVectors[N]): Int = vset.columns.head.dimension() //just get length of any column

          def identity(size: Int): SetOfVectors[N] ={
               val list = ListBuffer.fill[N](size, size)(Number.ZERO[N])

               for(r <- 0 until size) {
                    for(c <- 0 until size)
                         if(r == c)
                              list(r)(c) = Number.ONE[N]
               }
               SetOfVectors.fromSeqs(list:_*)
          }

          def rowReducedEchelon(v: SetOfVectors[N]): SetOfVectors[N] = ??? //todo

          //util things
          def getRows(vset: SetOfVectors[N]): ListBuffer[Vector[N]] = ??? //todo
          def getRow(vset: SetOfVectors[N], rowIndex: Int): Vector[N] = getRows(vset)(rowIndex)

          def getColumns(vset: SetOfVectors[N]): ListBuffer[Vector[N]] = ListBuffer(vset.columns:_*)
          def getColumn(vset: SetOfVectors[N], colIndex: Int): Vector[N] = getColumns(vset)(colIndex)

          //todo may not work if vec set doesn't work!
          def set(vset: SetOfVectors[N], rowIndex: Int, colIndex:Int, value: N): Unit ={
               vset.columns(colIndex).set(rowIndex, value)
          }

          def numRows(vset: SetOfVectors[N]): Int = dimension(vset)
          def numCols(vset: SetOfVectors[N]): Int = vset.columns.length
     }
}



class SetOfVectors[N: Number](val columns: Vector[N]*)


object SetOfVectors {

     def apply[N: Number](cols: Vector[N]*): SetOfVectors[N] = new SetOfVectors(cols:_*)

     def apply[N: Number](nr:Int, nc:Int): SetOfVectors[N] =
          SetOfVectors(Vector(ListBuffer.fill[N](nr * nc)(Number.ZERO[N]):_*))

     def ZERO[N: Number](numCols: Int, numRows: Int): SetOfVectors[N] =
          SetOfVectors.fromSeqs(Seq.fill[N](numCols, numRows)(Number.ZERO[N]):_*)

     def ONE[N: Number](numCols: Int, numRows: Int): SetOfVectors[N] =
          SetOfVectors.fromSeqs(Seq.fill[N](numCols, numRows)(Number.ONE[N]):_*)

     def IDENTITY[N: Number](size: Int)(implicit ev: SetVecLike[SetOfVectors[N], N]): SetOfVectors[N] =
          ev.identity(size)

     def IDENTITY[N: Number](vset: SetOfVectors[N])(implicit ev: SetVecLike[SetOfVectors[N], N],
                                                    dim: Dimension[SetOfVectors[N]]): SetOfVectors[N] =
          ev.identity(vset.dimension())

     def fromSeqs[N: Number](seqs: Seq[N]*): SetOfVectors[N] =
          SetOfVectors(seqs.map(aSeq => Vector(aSeq:_*)):_*)

     //assume data is along column
     def fromSingleSeq[N: Number](numRows: Int, numCols: Int, seq: Seq[N]): SetOfVectors[N] =
          fromSeqs(seq.grouped(numCols).toList:_*) //.toList.map(_.toList)

}





object SetVecTester extends App {

     import SetVecLike._

     val s1: SetOfVectors[Double] = SetOfVectors(Vector(1,2,3,4,5), Vector(8,8,1,2,3),
          Vector(-8,9,-3,0,1))
     println(s1.get(1,2))
     s1.set(0,0)(111)
     println(s1.get(0,0))
}
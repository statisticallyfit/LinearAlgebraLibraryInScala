package linalg.vector

import linalg.numeric._
import linalg.numeric.Number._
import linalg.theory.space._
import linalg.theory.basis._
import linalg.syntax.NumberSyntax._
import linalg.syntax.DimensionSyntax._
import linalg.vector.VectorLike._
import linalg.syntax.VectorLikeSyntax._
import linalg.syntax.VectorSpaceSyntax._
import linalg.syntax.SetVecLikeSyntax._
import linalg.util.Util

import scala.collection.mutable.{ListBuffer, Seq}
import scala.language.higherKinds
import scala.language.implicitConversions
import scala.util.control.Breaks._

/**
  *
  */


trait SetVecLike[V, F] extends VectorSpace[V, F]{

     def identity(size: Int): V
     def rowReducedEchelon(m: V): V
     def minus(v: V, w: V): V = plus(v, negate(w))
     def isZero(v: V): Boolean

     /*def copy(v: V): V

     def numRows(v: V): Int
     def numCols(v: V): Int

     def getRows(v: V): ListBuffer[Vector[F]]
     def getRow(v: V, rowIndex: Int): Vector[F] //todo ok? will polymorph e.g. polynomial?

     def getColumns(v: V): ListBuffer[Vector[F]]
     def getColumn(v: V, colIndex: Int): Vector[F]

     def get(v: V, rowIndex: Int, colIndex: Int)(implicit vec: VectorLike[Vector[F],F]): F =
          vec.get(getRow(v, rowIndex), colIndex) //todo why doesn't implicit work?


     /*def setRow(v: V, rowIndex: Int): Unit
     def setColumn(v: V, colIndex: Int): Unit*/ //todo major
     def set(v: V, rowIndex: Int, colIndex: Int, value: F): Unit*/
}


object SetVecLike {

     implicit def VectorSetIsVectorSetLike[V[_], N: Number: Trig: Absolute: Root: Compare](implicit vecLike:
     VectorSpace[V[N], N]) = new SetVecLike[SetOfVectors[N], N] with Dimension[SetOfVectors[N]] {

          val zero: SetOfVectors[N] = SetOfVectors(Vector.ZERO[N](1))
          val one: SetOfVectors[N] = SetOfVectors(Vector.ONE[N](1))

          def plus(vset: SetOfVectors[N], wset: SetOfVectors[N]): SetOfVectors[N] ={
               Util.Gen.ensureSize(vset, wset)
               SetOfVectors(vset.columns.zip(wset.columns).map(colPair => colPair._1 + colPair._2):_*)
          }

          def negate(vset: SetOfVectors[N]): SetOfVectors[N] = SetOfVectors(vset.columns.map(c => c.negate()):_*)

          def scale(v: SetOfVectors[N], factor: N): SetOfVectors[N] =
               SetOfVectors(v.columns.map(col => col.scale(factor)):_*)

          def isZero(v: SetOfVectors[N]): Boolean = v.columns.forall(col => col.isZero)


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



          //util things
          /*def copy(vset: SetOfVectors[N]): SetOfVectors[N] = SetOfVectors(vset.columns:_*)

          def getRow(vset: SetOfVectors[N], rowIndex: Int): Vector[N] = getRows(vset)(rowIndex)
          def getRows(vset: SetOfVectors[N]): ListBuffer[Vector[N]] = {
               val rows: ListBuffer[Vector[N]] = ListBuffer()
               for(r <- 0 until numRows(vset)) rows += Vector(vset.columns.map(colVec => colVec.get(r)):_*)
               rows
          }

          def getColumns(vset: SetOfVectors[N]): ListBuffer[Vector[N]] = ListBuffer(vset.columns:_*)
          def getColumn(vset: SetOfVectors[N], colIndex: Int): Vector[N] = getColumns(vset)(colIndex)

          //todo may not work if vec set doesn't work!
          def set(vset: SetOfVectors[N], rowIndex: Int, colIndex:Int, value: N): Unit ={
               vset.columns(colIndex).set(rowIndex)(value)
          }

          def numRows(vset: SetOfVectors[N]): Int = dimension(vset)
          def numCols(vset: SetOfVectors[N]): Int = vset.columns.length*/



          //todo put in implicit class
          def rowEchelon(vset: SetOfVectors[N]): SetOfVectors[N] = rref(vset, reduced = false)

          def rowReducedEchelon(vset: SetOfVectors[N]): SetOfVectors[N] = {
               val theRRef: SetOfVectors[N] = rref(vset, reduced=true)
               SetOfVectors(Util.Gen.expressRowsAsCols(Util.Gen.getNonZeroRows(theRRef)):_*)
          }

          private def rref(vset: SetOfVectors[N], reduced: Boolean): SetOfVectors[N] ={
               var echelonMatrix: SetOfVectors[N] = copy(vset)
               var lead: Int = 0
               val nRows: Int = numRows(vset)
               val nCols: Int = numCols(vset)

               breakable {
                    for(r <- 0 until nRows){
                         if(lead >= nCols){
                              break
                         }
                         var i: Int = r
                         while (get(echelonMatrix, i, lead).isZero) { //then find the pivot element
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
                         echelonMatrix = Util.Gen.scaleRow(r, get(echelonMatrix, r, lead).inverse(), echelonMatrix)

                         for(j <- 0 until nRows){ //back-substitute upwards
                              if(j != r){  //subtract row r * -rref[j][lead] from row j
                                   echelonMatrix = Util.Gen.sumRows(j, r,
                                        get(echelonMatrix, j, lead).negate(),
                                        echelonMatrix)
                              }
                         }

                         lead = lead + 1 //now looking for a pivot further to the right
                    }
               }

               echelonMatrix
          }
     }
}



case class SetOfVectors[N: Number](columns: Vector[N]*)(implicit d: Dimension[SetOfVectors[N]]) {

     import SetVecLike._


     def copy(): SetOfVectors[N] = SetOfVectors(this.columns:_*)
     def copy(cols: Seq[Vector[N]]): SetOfVectors[N] = SetOfVectors(cols:_*)

     def getRow(rowIndex: Int): Vector[N] = this.getRows()(rowIndex)

     def getRows(): Seq[Vector[N]] = {
          val rows: ListBuffer[Vector[N]] = ListBuffer()
          for(r <- 0 until this.numRows) rows += Vector(columns.map(colVec => colVec.get(r)):_*)
          Seq(rows:_*)
     }

     def getColumns(): Seq[Vector[N]] = Seq(columns:_*)
     def getColumn(colIndex: Int): Vector[N] = columns(colIndex)

     def set(rowIndex: Int, colIndex:Int)(value: N): Unit = columns(colIndex).set(rowIndex)(value)
     def get(rowIndex: Int, colIndex: Int): N = this.getRow(rowIndex).get(colIndex)

     def numRows: Int = this.dimension() //d.dimension(this)
     def numCols: Int = columns.length

     //tostring todo
}


object SetOfVectors {

     //def apply[N: Number:Trig:Root:Absolute:Compare](cols: Vector[N]*): SetOfVectors[N] = new SetOfVectors(cols:_*)

     def apply[N: Number:Trig:Root:Absolute:Compare](nr:Int, nc:Int): SetOfVectors[N] =
          new SetOfVectors(Vector(ListBuffer.fill[N](nr * nc)(Number.ZERO[N]):_*))

     def ZERO[N: Number:Trig:Root:Absolute:Compare](numCols: Int, numRows: Int): SetOfVectors[N] =
          SetOfVectors.fromSeqs(Seq.fill[N](numCols, numRows)(Number.ZERO[N]):_*)

     def ONE[N: Number:Trig:Root:Absolute:Compare](numCols: Int, numRows: Int): SetOfVectors[N] =
          SetOfVectors.fromSeqs(Seq.fill[N](numCols, numRows)(Number.ONE[N]):_*)

     def IDENTITY[N: Number:Trig:Root:Absolute:Compare](size: Int)
                                                       (implicit ev: SetVecLike[SetOfVectors[N], N]):SetOfVectors[N] =
          ev.identity(size)

     def IDENTITY[N: Number:Trig:Root:Absolute:Compare](vset: SetOfVectors[N])
                                                       (implicit ev: SetVecLike[SetOfVectors[N], N],
                                                        dim: Dimension[SetOfVectors[N]]): SetOfVectors[N] =
          ev.identity(vset.dimension())

     def fromSeqs[N: Number:Trig:Root:Absolute:Compare](seqs: Seq[N]*): SetOfVectors[N] =
          SetOfVectors(seqs.map(aSeq => Vector(aSeq:_*)):_*)

     //assume data is along column
     def fromSingleSeq[N: Number:Trig:Root:Absolute:Compare](numRows: Int, numCols: Int, seq: Seq[N]): SetOfVectors[N] =
          fromSeqs(seq.grouped(numCols).toList:_*) //.toList.map(_.toList)

}





object SetVecTester extends App {

     import SetVecLike._

     val s1: SetOfVectors[Double] = SetOfVectors(Vector(1,2,3,4,5), Vector(8,8,1,2,3),
          Vector(-8,9,-3,0,1))
     println(s1.get(1,2)) //should be 9
     s1.getColumn(1).set(1)(333)
     s1.set(0,0)(111)
     println(s1)
     println(s1.get(0,0))

     s1.copy()
}
package linalg.vector

import linalg.numeric._
import linalg.numeric.Number._
import linalg.theory.space._
import linalg.theory.basis._
import linalg.syntax.NumberSyntax._
import linalg.syntax.DimensionSyntax._
import linalg.vector.VectorLike._
import linalg.syntax.VectorSpaceSyntax._
import linalg.show.Show
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
     def rowEchelon(m: V): V
     def minus(v: V, w: V): V = plus(v, negate(w))
     def isZero(v: V): Boolean
}


object SetVecLike {

     /*(implicit vecLike: VectorLike[V[N], N])*/
     implicit def SetVecsSetVecLike[N: Number] = new SetVecLike[SetOfVectors[N], N] with Dimension[SetOfVectors[N]] {

          val zero: SetOfVectors[N] = SetOfVectors(Vector.ZERO[N](1))
          val one: SetOfVectors[N] = SetOfVectors(Vector.ONE[N](1))

          def plus(vset: SetOfVectors[N], wset: SetOfVectors[N]): SetOfVectors[N] ={
               Util.Gen.ensureSize(vset, wset)
               SetOfVectors(vset.getColumns().zip(wset.getColumns()).map(colPair => colPair._1 + colPair._2):_*)
          }

          def negate(vset: SetOfVectors[N]): SetOfVectors[N] = SetOfVectors(vset.getColumns().map(c => c.negate()):_*)

          def scale(v: SetOfVectors[N], factor: N): SetOfVectors[N] =
               SetOfVectors(v.getColumns().map(col => col.scale(factor)):_*)

          def isZero(v: SetOfVectors[N]): Boolean = v.getColumns().forall(col => col.isZero)


          def dimension(vset: SetOfVectors[N]): Int = vset.getColumns().head.dimension() //just get length of any column

          def identity(size: Int): SetOfVectors[N] ={
               val list = ListBuffer.fill[N](size, size)(Number.ZERO[N])

               for(r <- 0 until size) {
                    for(c <- 0 until size)
                         if(r == c)
                              list(r)(c) = Number.ONE[N]
               }
               SetOfVectors.fromSeqs(list:_*)
          }


          def rowEchelon(vset: SetOfVectors[N]): SetOfVectors[N] = rref(vset, reduced = false)

          def rowReducedEchelon(vset: SetOfVectors[N]): SetOfVectors[N] = {
               val theRRef: SetOfVectors[N] = rref(vset, reduced=true)
               SetOfVectors(Util.Gen.expressRowsAsCols(Util.Gen.getNonZeroRows(theRRef)):_*)
          }

          private def rref(vset: SetOfVectors[N], reduced: Boolean): SetOfVectors[N] ={
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
}
import SetVecLike._



case class SetOfVectors[N: Number](private val cols: Vector[N]*) {

     private val columns: Seq[Vector[N]] = Seq(cols:_*)
     val numRows: Int = this.asInstanceOf[SetOfVectors[N]].dimension()
     val numCols: Int = columns.length

     private val s = implicitly[Show[SetOfVectors[N]]]

     def copy(): SetOfVectors[N] = SetOfVectors(columns:_*)
     def copy(cols: Seq[Vector[N]]): SetOfVectors[N] = SetOfVectors(cols:_*)


     def getColumns(): Seq[Vector[N]] = Seq(columns:_*)

     def getColumn(colIndex: Int): Vector[N] = columns(colIndex)

     def getRow(rowIndex: Int): Vector[N] = this.getRows()(rowIndex)

     def getRows(): Seq[Vector[N]] = {
          val rows: Seq[Vector[N]] = Seq()
          for(r <- 0 until this.numRows) rows(r) = Vector(columns.map(colVec => colVec.get(r)):_*)
          rows
     }

     def setColumn(colIndex: Int, col: Vector[N]): Unit = columns(colIndex) = col
     def setRow(rowIndex: Int, row: Vector[N]): Unit = {
          for(c <- 0 until numCols){
               this.set(rowIndex, c)(row.get(c))
          }
     }


     def get(rowIndex: Int, colIndex: Int): N = this.getRow(rowIndex).get(colIndex)
     def set(rowIndex: Int, colIndex:Int)(value: N): Unit = columns(colIndex).set(rowIndex)(value)



     override def toString: String = s.show(this)
}


object SetOfVectors {

     //def apply[N: Number:Trig:Root:Absolute:Compare](cols: Vector[N]*): SetOfVectors[N] = new SetOfVectors(cols:_*)

     def apply[N: Number](nr:Int, nc:Int): SetOfVectors[N] =
          new SetOfVectors(Vector(Seq.fill[N](nr * nc)(Number.ZERO[N]):_*))

     def ZERO[N: Number](numCols: Int, numRows: Int): SetOfVectors[N] =
          SetOfVectors.fromSeqs(Seq.fill[N](numCols, numRows)(Number.ZERO[N]):_*)

     def ONE[N: Number](numCols: Int, numRows: Int): SetOfVectors[N] =
          SetOfVectors.fromSeqs(Seq.fill[N](numCols, numRows)(Number.ONE[N]):_*)

     def IDENTITY[N: Number](size: Int)(implicit ev: SetVecLike[SetOfVectors[N], N]):SetOfVectors[N] =
          ev.identity(size)

     def IDENTITY[N: Number](vset: SetOfVectors[N])(implicit ev: SetVecLike[SetOfVectors[N], N]): SetOfVectors[N] =
          ev.identity(vset.dimension())

     def fromSeqs[N: Number](seqs: Seq[N]*): SetOfVectors[N] = SetOfVectors(seqs.map(aSeq => Vector(aSeq:_*)):_*)

     //assume data is along column
     def fromSingleSeq[N: Number](numRows: Int, numCols: Int, seq: Seq[N]): SetOfVectors[N] =
          fromSeqs(seq.grouped(numCols).toList:_*) //.toList.map(_.toList)

}





object SetVecTester extends App {

     val s1: SetOfVectors[Double] = SetOfVectors(Vector(1,2,3,4,5), Vector(8,8,1,2,3),
          Vector(-8,9,-3,0,1))
     println(s1.get(1,2)) //should be 9
     s1.getColumn(1).set(1)(333)
     s1.set(0,0)(111)
     println(s1)
     println(s1.get(0,0))

     s1.copy()
}
package linalg.instances

import linalg.implicits._
import linalg.kernel._
import linalg.theory._
import linalg.theory.space._
import linalg.vector._
import linalg.util.Util

import scala.collection.mutable.{ListBuffer, Seq}
import scala.util.control.Breaks._

/**
  *
  */
//import spire.algebra.InnerProductSpace

trait VectorIsVectorSpace

trait VectorSpaceInstances {

     implicit def vectorIsVectorSpace[N: Number] = new VectorSpace[Vector[N], N] {
          val zero: Vector[N] = Vector(Number.ZERO[N]) //just vector with one element
          val one: Vector[N] = Vector(Number.ONE[N]) //just vector with one element


          def plus(v: Vector[N], w: Vector[N]): Vector[N] ={
               Util.Gen.ensureSize(v, w)
               Vector(v.getElements().zip(w.getElements()).map(pair => pair._1 + pair._2):_*)
          }

          def negate(v: Vector[N]): Vector[N] = Vector(v.getElements().map(e => e.negate()):_*)

          def scale(v: Vector[N], factor: N): Vector[N] = Vector(v.getElements().map(e => e * factor):_*)

     }
}


trait temp {
     /*(implicit vecLike: VectorLike[V[N], N])*/
     implicit def VecSetIsSetVecLike[N: Number] = new SetVecLike[SetOfVectors[N], N] {

          val zero: SetOfVectors[N] = SetOfVectors(Vector.ZERO[N](1))
          val one: SetOfVectors[N] = SetOfVectors(Vector.ONE[N](1))

          /** Vector space part */ // ---------------------------------------------------------------------------------
          def plus(vset: SetOfVectors[N], wset: SetOfVectors[N]): SetOfVectors[N] ={
               Util.Gen.ensureSize(vset, wset)
               SetOfVectors(vset.getColumns().zip(wset.getColumns()).map(colPair => colPair._1 + colPair._2):_*)
          }

          def negate(vset: SetOfVectors[N]): SetOfVectors[N] = SetOfVectors(vset.getColumns().map(c => c.negate()):_*)

          def scale(v: SetOfVectors[N], factor: N): SetOfVectors[N] =
               SetOfVectors(v.getColumns().map(col => col.scale(factor)):_*)



          /** Set vec part */ // ---------------------------------------------------------------------------------
          def isZero(v: SetOfVectors[N]): Boolean = v.getColumns().forall(col => col.isZero)

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
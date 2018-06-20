package linalg.instances.linear

import linalg.implicits._
import linalg._
/*import linalg.kernel._
import linalg.theory._
import linalg.theory.space._
import linalg.theory.basis._*/

import scala.language.higherKinds
import scala.language.implicitConversions
//import linalg.kernel._ //{Number, RealNumber, Root, Trig}
/*import linalg.theory.basis.Dimension
import linalg.theory.{AbelianGroup, Field, Monoid}
import linalg.theory.space.{HilbertSpace, InnerProductSpace, NormedVectorSpace, VectorSpace}*/
//import linalg.util.Util
import linalg.vector.{SetOfVectors, Vector}
//import linalg.vector.SetVecLike
import linalg.util._

import scala.collection.mutable.{ListBuffer, Seq}
import scala.util.control.Breaks.{break, breakable}
/**
  *
  */
class SetVecThings[N: Number] {

     class SetVecIsMonoid extends Monoid[SetOfVectors[N]]{

          val zero: SetOfVectors[N] = SetOfVectors(Vector.ZERO[N](1))

          def plus(vset: SetOfVectors[N], wset: SetOfVectors[N]): SetOfVectors[N] ={
               Util.Gen.ensureSize(vset, wset)
               SetOfVectors(vset.getColumns().zip(wset.getColumns())
                    .map(colPair => colPair._1 + colPair._2):_*)
          }
     }

     class SetVecIsAbelianGroup extends SetVecIsMonoid with AbelianGroup[SetOfVectors[N]]{
          def negate(vset: SetOfVectors[N]): SetOfVectors[N] = SetOfVectors(vset.getColumns().map(c => c.negate()):_*)
     }

     class SetVecIsVectorSpace extends SetVecIsAbelianGroup with VectorSpace[SetOfVectors[N], N]{
          val one: SetOfVectors[N] = SetOfVectors(Vector.ONE[N](1))
          def scale(v: SetOfVectors[N], factor: N): SetOfVectors[N] =
               SetOfVectors(v.getColumns().map(col => col.scale(factor)):_*)
     }

     class SetVecIsSetVecLike extends SetVecIsVectorSpace with SetVecLike[SetOfVectors[N], N]{
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

     class SetVecHasDimension extends Dimension[SetOfVectors[N]]{
          def dimension(vset: SetOfVectors[N]): Int = vset.getColumns().head.dimension()
     }


     val monoid = new SetVecIsMonoid
     val abelian = new SetVecIsAbelianGroup
     val vectorSpace = new SetVecIsVectorSpace
     val vsetLike = new SetVecIsSetVecLike
     val dim: SetVecHasDimension = new SetVecHasDimension
}

trait SetVecInstances {

     implicit def setVecIsMonoid[N: Number] = new SetVecThings[N].monoid
     implicit def setVecIsAbelianGroup[N: Number] = new SetVecThings[N].abelian
     implicit def setVecIsVectorSpace[N: Number] = new SetVecThings[N].vectorSpace
     implicit def setVecIsSetVecLike[N: Number] = new SetVecThings[N].vsetLike
     implicit def setVecHasDimension[N: Number] = new SetVecThings[N].dim
}

package linalg.instances

import cats.Eq
import cats.implicits._
//import linalg.kernel._
//import linalg.{NRoot, Number, RealNumber, Trig}

//import linalg.kernel.{NRoot, Number, RealNumber, Trig}
//import linalg.theory._
//import linalg.theory.space._
import linalg._

import linalg.util.Util
import linalg.implicits._
import linalg.vector.{SetOfVectors, Vector}

import scala.collection.mutable.{ListBuffer, Seq}
import scala.util.control.Breaks.{break, breakable}

/**
  *
  */
//import spire.algebra.InnerProductSpace

trait VectorLikeInstances extends VectorSpaceInstances {

     implicit def VectorIsVectorLike[N: Number] = new VectorLike[Vector[N], N] {
          /*with Span[Vector[N], N]*/

          def isZero(v: Vector[N]): Boolean = v.getElements().forall(e => e :==: Number[N].zero)

          def projection[R:RealNumber](v: Vector[N], onto: Vector[N])(implicit field: Field[N],
                                                                      r: RootLike[N,R]): Vector[N] =
               scale(onto, field.divide(innerProduct(v, onto), norm[R](onto)) )

          def outerProduct(v: Vector[N], w: Vector[N]): SetOfVectors[N] = {
               Util.Gen.ensureSize(v, w)

               val as: Seq[N] = v.getElements()
               val bs: Seq[N] = w.getElements()

               val result: Seq[Seq[N]] = as.map(a => bs.map(b => a * b))

               SetOfVectors.fromSeqs(result:_*)
          }

          def innerProduct(v: Vector[N], w: Vector[N]): N = {
               Util.Gen.ensureSize(v, w)
               v.getElements().zip(w.getElements()).map(pair => pair._1 * pair._2).reduceLeft((acc, y) => acc + y)
          }

          def crossProduct(u: Vector[N], v: Vector[N]): Option[Vector[N]] = {

               if(u.dimension() == 3 && v.dimension() == 3){
                    val w1: N = (u.get(2) * v.get(3)) - (u.get(3) * v.get(2))
                    val w2: N = (u.get(3) * v.get(1)) - (u.get(1) * v.get(3))
                    val w3: N = (u.get(1) * v.get(2)) - (u.get(2) * v.get(1))

                    Some(Vector(w1, w2, w3))

               } else None
          }

          def angle[R:RealNumber](v: Vector[N], w: Vector[N])(implicit t: Trig[N],
                                                              field: Field[N], r: RootLike[N,R]): N =
               field.divide(innerProduct(v, w),  field.times(norm[R](v), norm[R](w)).arccos() )

          def norm[R:RealNumber](v: Vector[N])(implicit field: Field[N], root: RootLike[N, R]): N =
               Util.Gen.total[N](Vector[N](v.getElements().map(e => root.power(e, RealNumber[R].two)):_*))
          //goal: Util.Gen.total[N](v.map(e => ...))
          //todo test if this sums well, or if need explicit: .reduceLeft[N]((acc, y) => field.plus(acc,y))
     }
}
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


     //todo decision - either remove inheritance from vectorlike of vecspace
     // todo - or you keep inheritance. Try without inheritance to see if you need to
     //have too many implicits in parameters like Number:Trig:Root etc ...

     //todo maybe error because need to move it in another VecLike instances file????







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

package linalg.instances.linear

import linalg.implicits._
import linalg._
import linalg.vector.{SetOfVectors, Vector}
import linalg.util._
import spire.algebra.Eq

import scala.language.higherKinds
import scala.language.implicitConversions
import scala.collection.mutable.{ListBuffer, Seq}
import scala.util.control.Breaks.{break, breakable}
/**
  *
  */
class SetVecThings[N: Number] {

     //TODO make absolute and Root instances.

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
               val list = ListBuffer.fill[N](size, size)(Number[N].zero)

               for(r <- 0 until size) {
                    for(c <- 0 until size)
                         if(r == c)
                              list(r)(c) = Number.ONE[N]
               }
               SetOfVectors.fromSeqs(list:_*)
          }


          def rowEchelon(vset: SetOfVectors[N]): SetOfVectors[N] = Util.Gen.rowEchelon[N](vset)

          def rowReducedEchelon(vset: SetOfVectors[N]): SetOfVectors[N] = Util.Gen.rowReducedEchelon[N](vset)

     }

     class SetVecHasDimension extends Dimension[SetOfVectors[N]]{
          def dimension(vset: SetOfVectors[N]): Int = vset.getColumns().head.dimension()
     }

     class SetVecHasEq extends Eq[SetOfVectors[N]]{
          def eqv(vset: SetOfVectors[N], wset: SetOfVectors[N]): Boolean = {
               Util.Gen.ensureSize(vset, wset)

               vset.getColumns()
                    .zip(wset.getColumns())
                    .forall(colPair => Eq[Vector[N]].eqv(colPair._1, colPair._2))
          }
     }


     class SetVecSpan extends SetVecIsSetVecLike with Span[Vector[N], SetOfVectors[N], N]{

          //TODO why doesn't implicit syntax work , why have to call rref with vv. ??
          //private val vv: SetVecLike[SetOfVectors[N], N] = implicitly[SetVecLike[SetOfVectors[N], N]]


          def span(vset: SetOfVectors[N]): SetOfVectors[N] = ??? //vv.rowReducedEchelon(vset)

          def doesSetSpanTheSpace(vset: SetOfVectors[N]): Boolean ={
               ???
          }

          def doesSetSpanTheVector(vset: SetOfVectors[N], v: Vector[N]): Boolean ={
               ???
          }

          def getSpanningCoefficients(vset: SetOfVectors[N], v: Vector[N]): Option[List[N]] ={
               ???
          }
     }

     val eq = new SetVecHasEq
     val monoid = new SetVecIsMonoid
     val abelian = new SetVecIsAbelianGroup
     val vectorSpace = new SetVecIsVectorSpace
     val vsetLike = new SetVecIsSetVecLike
     val dim = new SetVecHasDimension
}

trait SetVecInstances {

     implicit final def setVecHasEq[N: Number] = new SetVecThings[N].eq
     implicit final def setVecIsMonoid[N: Number] = new SetVecThings[N].monoid
     implicit final def setVecIsAbelianGroup[N: Number] = new SetVecThings[N].abelian
     implicit final def setVecIsVectorSpace[N: Number] = new SetVecThings[N].vectorSpace
     implicit final def setVecIsSetVecLike[N: Number] = new SetVecThings[N].vsetLike
     implicit final def setVecHasDimension[N: Number] = new SetVecThings[N].dim
}

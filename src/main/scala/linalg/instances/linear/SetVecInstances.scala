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



import shapeless._
/**
  *
  */

object SetVecLikeComp {

     implicit def setVecForMonoidHList[N: Number]: Monoid[SetOfVectors[N] :: HNil] =
          new Monoid[SetOfVectors[N] :: HNil] {

     }
     implicit def setVecForSetVecLikeHList[N: Number]: SetVecLike[SetOfVectors[N] :: HNil, N] = new
               SetVecLike[SetOfVectors[N] :: HNil, N] {


          def isZero(v: SetOfVectors[N] :: HNil): Boolean = v.head.getColumns().forall(col => col.isZero)

          def identity(size: Int): SetOfVectors[N] :: HNil ={
               val list = ListBuffer.fill[N](size, size)(Number[N].zero)

               for(r <- 0 until size) {
                    for(c <- 0 until size)
                         if(r == c)
                              list(r)(c) = Number.ONE[N]
               }
               SetOfVectors.fromSeqs(list:_*) :: HNil
          }


          def rowEchelon(vset: SetOfVectors[N] :: HNil): SetOfVectors[N] :: HNil = Util.Gen.rowEchelon[N](vset.head):: HNil

          def rowReducedEchelon(vset: SetOfVectors[N] :: HNil): SetOfVectors[N] :: HNil =
               Util.Gen.rowReducedEchelon[N](vset.head) :: HNil

          //vec space
          val zero: SetOfVectors[N] :: HNil = SetOfVectors(Vector.ZERO[N](1)) :: HNil
          val one: SetOfVectors[N] :: HNil = SetOfVectors(Vector.ONE[N](1)) :: HNil

          def scale(v: SetOfVectors[N] :: HNil, factor: N): SetOfVectors[N] :: HNil =
               SetOfVectors(v.head.getColumns().map(col => col.scale(factor)):_*) :: HNil

          def plus(vset: SetOfVectors[N] :: HNil, wset: SetOfVectors[N] :: HNil): SetOfVectors[N] :: HNil ={
               Util.Gen.ensureSize(vset.head, wset.head)
               SetOfVectors(vset.head.getColumns().zip(wset.head.getColumns())
                    .map(colPair => colPair._1 + colPair._2):_*) :: HNil
          }
          def negate(vset: SetOfVectors[N] :: HNil): SetOfVectors[N] :: HNil =
               SetOfVectors(vset.head.getColumns().map(c => c.negate()):_*) :: HNil
     }


     implicit def genericSetVecLike[V, R, N: Number](implicit gen: Generic.Aux[V, R],
                                                     slike: SetVecLike[R, N]): SetVecLike[V, N] = new SetVecLike[V, N] {

          override def minus(vset1: V, vset2: V): V = gen.from(slike.minus(gen.to(vset1), gen.to(vset2)))
          override def rowEchelon(vset: V): V = gen.from(slike.rowEchelon(gen.to(vset)))
          override def rowReducedEchelon(vset: V) = gen.from(slike.rowReducedEchelon(gen.to(vset)))

          override def isZero(vset: V): Boolean = slike.isZero(gen.to(vset))
          override def identity(size: Int): V = gen.from(slike.identity(size))

          // ---- vec space things
          override val zero: V = gen.from(slike.zero)
          override val one: V = gen.from(slike.one)

          override def plus(vset1: V, vset2: V): V = gen.from(slike.plus(gen.to(vset1), gen.to(vset2)))
          override def scale(vset: V, constant: N ): V = gen.from(slike.scale(gen.to(vset), constant))
          override def negate(vset: V): V = gen.from(slike.negate(gen.to(vset)))
     }

     //The way to generate an Amount type class instance for HLists so that the resulting typeclass
     // can be plugged into genericAmount[A, R] where R is the HList.
     implicit def setVecLikeForHList[H, T <: HList, N: Number](
                                                                   implicit slike: Lazy[SetVecLike[H, N]], defaultTail: DefaultInstance[T]
                                                              ): SetVecLike[H :: T, N] = new SetVecLike[H :: T, N] {

          override def minus(vset1: H :: T, vset2: H :: T): H :: T =
               slike.value.minus(vset1.head, vset2.head) :: defaultTail.instance

          override def rowEchelon(vset: H :: T): H :: T = slike.value.rowEchelon(vset.head) :: defaultTail.instance
          override def rowReducedEchelon(vset: H :: T): H :: T = slike.value.rowReducedEchelon(vset.head) :: defaultTail
               .instance
          override def isZero(vset: H :: T): Boolean = slike.value.isZero(vset.head)
          override def identity(size: Int): H :: T = slike.value.identity(size) :: defaultTail.instance


          // --- vec space
          override val zero: H :: T = slike.value.zero  :: defaultTail.instance
          override val one: H :: T = slike.value.one :: defaultTail.instance

          override def plus(vset1: H :: T, vset2: H :: T): H :: T =
               slike.value.plus(vset1.head, vset2.head) :: defaultTail.instance
          override def scale(vset: H :: T, constant: N): H :: T = slike.value.scale(vset.head, constant) :: defaultTail.instance
          override def negate(vset: H :: T): H :: T = slike.value.negate(vset.head) :: defaultTail.instance
     }

}



trait DefaultInstance[T] {
     def instance: T
}

object DefaultInstance {

     def apply[A](a: A): DefaultInstance[A] = new DefaultInstance[A] {
          override def instance = a
     }

     implicit def defaultInstanceForHList[H, T <: HList](
                                                             implicit defaultHead: DefaultInstance[H], defaultTail: DefaultInstance[T]
                                                        ): DefaultInstance[H :: T] = new DefaultInstance[H :: T] {
          override def instance: H :: T = defaultHead.instance :: defaultTail.instance
     }

     // Now making the stop case - defaultinstance for hnil
     implicit val defaultInstanceForHNil: DefaultInstance[HNil] = new DefaultInstance[HNil] {
          override def instance: HNil = HNil
     }
}



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

package linalg.instances.linear

import linalg.implicits._
import linalg._
import linalg.matrix.{AugmentedMatrix, Matrix}
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

     //TODO
     /*class SetVecHasAbsoluteValue extends Absolute[SetOfVectors[N], SetOfVectors[N]] {

          def absoluteValue(vset: SetOfVectors[N]): SetOfVectors[N] =
               SetOfVectors(vset.getColumns().map(vec => vec.abs()):_*)
     }*/

     class SetVecIsMonoid extends Monoid[SetOfVectors[N]]{

          val zero: SetOfVectors[N] = SetOfVectors(Vector.ZERO[N](1))

          def plus(vset: SetOfVectors[N], wset: SetOfVectors[N]): SetOfVectors[N] =
               Util.plus(vset, wset)
     }

     class SetVecIsAbelianGroup extends SetVecIsMonoid with AbelianGroup[SetOfVectors[N]]{
          def negate(vset: SetOfVectors[N]): SetOfVectors[N] = Util.negate(vset)
     }

     class SetVecIsVectorSpace extends SetVecIsAbelianGroup with VectorSpace[SetOfVectors[N], N]{
          val one: SetOfVectors[N] = SetOfVectors(Vector.ONE[N](1))
          def scale(vset: SetOfVectors[N], factor: N): SetOfVectors[N] = Util.scale(vset, factor)
     }

     class SetVecIsSetVecLike extends SetVecIsVectorSpace with SetVecLike[SetOfVectors[N], N]{
          def isZero(vset: SetOfVectors[N]): Boolean = Util.isZero(vset)
          def rowReducedEchelon(vset: SetOfVectors[N]): SetOfVectors[N] = Util.rowReducedEchelon[N](vset)
          def rowEchelon(mat: SetOfVectors[N]): SetOfVectors[N] = Util.rowEchelon(mat)
          def size(mat: SetOfVectors[N]): (Int, Int) = Util.size(mat)
          def transpose(vset: SetOfVectors[N]): SetOfVectors[N] = Util.transpose(vset)
     }

     class SetVecHasDimension extends Dimension[SetOfVectors[N]]{
          def dimension(vset: SetOfVectors[N]): Int = Util.dimension(vset)
     }

     class SetVecHasEq extends Eq[SetOfVectors[N]]{
          def eqv(vset: SetOfVectors[N], wset: SetOfVectors[N]): Boolean = Util.eqv(vset, wset)
     }

     class SetVecSpan extends SetVecIsSetVecLike with Span[Vector[N], SetOfVectors[N], N]{

          def span(vset: SetOfVectors[N]): SetOfVectors[N] = Util.span(vset)
          def doesSetSpanTheSpace(vset: SetOfVectors[N]): Boolean = Util.doesSetSpanTheSpace(vset)
          def doesSetSpanTheVector(vset: SetOfVectors[N], v: Vector[N]): Boolean =
               Util.doesSetSpanTheVector(vset, v)
          def getSpanningCoefficients(vset: SetOfVectors[N], v: Vector[N]): Option[Matrix[N]] =
               Util.getSpanningCoefficients(vset, v)
     }

     //TODO ATTENTION!!! rowreducedechelon is implemented by removing zero rows at the end
     // TODO so need to fix that because a vset is only a basis of a space R^n if it is n by n and
     // TODO the cols are linealry independent.

     class SetVecBasis extends Basis[Vector[N], SetOfVectors[N], N]{
          //TODO check example 8 howard if impl. is truly correct ...
          def basisOfSpaceSpannedBySet(vset: SetOfVectors[N]): SetOfVectors[N] =
               Util.basisOfSpaceSpannedBySet(vset)
          def alternateBasisOfSpaceSpannedBySet(vset: SetOfVectors[N]): SetOfVectors[N] =
               Util.alternateBasisOfSpaceSpannedBySet(vset)
          def isBasisOfSet(vset: SetOfVectors[N], maybeBasis: SetOfVectors[N]): Boolean =
               Util.isBasisOfSet(vset, maybeBasis)

          //def basisOfSpace(vset: SetOfVectors[N]): SetOfVectors[N] = Util.basisOfSpace(vset)
          def isBasisOfSpace(vset: SetOfVectors[N]): Boolean = Util.isBasisOfSpace(vset)

          def isBasisOfVector(vset: SetOfVectors[N], v: Vector[N]): Boolean =
               Util.doesSetSpanTheVector(vset, v)
     }

     class SetVecCanHaveLinearIndependence extends LinearIndependence[SetOfVectors[N]] {

          def linearlyIndependent(vset: SetOfVectors[N], wset: SetOfVectors[N]): Boolean =
               Util.linearlyIndependent(vset, wset)
          def isLinearlyIndependent(vset: SetOfVectors[N]): Boolean = Util.isLinearlyIndependent(vset)
     }

     class SetVecHasRowSpace extends RowSpace[Vector[N], SetOfVectors[N], N] {

          def isInRowSpace(vset: SetOfVectors[N], v: Vector[N]): Boolean =
               Util.isInRowSpace(vset, v)
          def equalRowSpaces(vset1: SetOfVectors[N], vset2: SetOfVectors[N]): Boolean =
               Util.equalRowSpaces(vset1, vset2)
          def rowSpace(vset: SetOfVectors[N]): SetOfVectors[N] = Util.rowSpace(vset)
          def areRowsSpanningSpace(vset: SetOfVectors[N]): Boolean = Util.areRowsSpanningSpace(vset)

          def rank(vset: SetOfVectors[N]): Int = Util.rank(vset)
          def isFullRank(vset: SetOfVectors[N]): Boolean = Util.isFullRank(vset)
     }

     class SetVecHasColumnSpace extends ColumnSpace[Vector[N], SetOfVectors[N], N] {

          def isInColumnSpace(vset: SetOfVectors[N], v: Vector[N]): Boolean =
               Util.isInColumnSpace(vset, v)
          def equalColSpaces(vset1: SetOfVectors[N], vset2: SetOfVectors[N]): Boolean =
               Util.equalColSpaces(vset1, vset2)
          def columnSpace(vset: SetOfVectors[N]): SetOfVectors[N] = Util.columnSpace(vset)
          def areColsSpanningSpace(vset: SetOfVectors[N]): Boolean = Util.areColsSpanningSpace(vset)
          def columnRank(vset: SetOfVectors[N]): Int = Util.columnRank(vset)
     }

     class SetVecHasNullSpace extends NullSpace[Vector[N], SetOfVectors[N], N] {
          def isInNullSpace(vset: SetOfVectors[N], v: Vector[N]): Boolean =
               Util.isInNullSpace(vset, v)
          def equalNullSpaces(vset1: SetOfVectors[N], vset2: SetOfVectors[N]): Boolean =
               Util.equalNullSpaces(vset1, vset2)
          def nullSpace(vset: SetOfVectors[N]): SetOfVectors[N] = Util.nullSpace(vset)
          def nullity(vset: SetOfVectors[N]): Int = Util.nullity(vset)
          //TODO def areColsBasisOfSpace(): Boolean = ev.areColsBasisOfSpace(current)
          //TODO def areColsSpanningSpace(): Boolean = ev.areColsSpanningSpace(current)
     }

     //val absolute = new SetVecHasAbsoluteValue
     val eq = new SetVecHasEq
     val monoid = new SetVecIsMonoid
     val abelian = new SetVecIsAbelianGroup
     val vectorSpace = new SetVecIsVectorSpace
     val setVecLike = new SetVecIsSetVecLike
     val independence = new SetVecCanHaveLinearIndependence
     val span = new SetVecSpan
     val basis = new SetVecBasis
     val dim = new SetVecHasDimension
     val rowSpace = new SetVecHasRowSpace
     val colSpace = new SetVecHasColumnSpace
     val nullSpace = new SetVecHasNullSpace
}

trait SetVecInstances {

     //implicit final def setVecHasAbsoluteValue[N: Number] = new SetVecThings[N].absolute
     implicit final def setVecHasEq[N: Number] = new SetVecThings[N].eq
     implicit final def setVecHasDimension[N: Number] = new SetVecThings[N].dim
     implicit final def setVecIsMonoid[N: Number] = new SetVecThings[N].monoid
     implicit final def setVecIsAbelianGroup[N: Number] = new SetVecThings[N].abelian
     implicit final def setVecIsVectorSpace[N: Number] = new SetVecThings[N].vectorSpace
     implicit final def setVecIsSetVecLike[N: Number] = new SetVecThings[N].setVecLike
     implicit final def setVecHasSpan[N: Number] = new SetVecThings[N].span
     implicit final def setVecHasBasis[N: Number] = new SetVecThings[N].basis
     implicit final def setVecCanHaveLinearIndependence[N: Number] = new SetVecThings[N].independence
     implicit final def setVecHasRowSpace[N: Number] = new SetVecThings[N].rowSpace
     implicit final def setVecHasColSpace[N: Number] = new SetVecThings[N].colSpace
     implicit final def setVecHasNullSpace[N: Number] = new SetVecThings[N].nullSpace
}











/*
import shapeless._


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





//trait SetVecInstances {
class SetVecThings[N: Number] {


     // ---------

     class GenericMonoid[M, R](implicit gen: Generic.Aux[M, R], monoid: Monoid[R]) {

          val zero: M = gen.from(monoid.zero)
          def plus(vset1: M, vset2: M): M = gen.from(monoid.plus(gen.to(vset1), gen.to(vset2)))
     }

     class GenericAbelianGroup[A, R](implicit gen: Generic.Aux[A, R], abelian: AbelianGroup[R])
          extends GenericMonoid {

          def negate(vset: A): A = gen.from(abelian.negate(gen.to(vset)))
     }

     class GenericVectorSpace[V, R, F: Field](implicit gen: Generic.Aux[V, R], vs: VectorSpace[R, F])
          extends GenericAbelianGroup {
          /*implicit def genericVectorSpace[V, R, F: Field](implicit gen: Generic.Aux[V, R],
                                                          vs: VectorSpace[R, F]): VectorSpace[V, F] = new VectorSpace[V, F] {*/
          val one: V = gen.from(vs.one)
          def scale(vset: V, constant: F): V = gen.from(vs.scale(gen.to(vset), constant))
     }


     class GenericSetVecLike[V, R, F:Field](implicit gen: Generic.Aux[V, R],
                                              slike: SetVecLike[R, N]) extends GenericVectorSpace[V, R, F] {
          /*implicit def genericSetVecLike[V, R, N: Number](implicit gen: Generic.Aux[V, R],
                                                          slike: SetVecLike[R, N]): SetVecLike[V, N] = new SetVecLike[V, N] {*/

          //override def minus(vset1: V, vset2: V): V = gen.from(slike.minus(gen.to(vset1), gen.to(vset2)))
          def rowEchelon(vset: V): V = gen.from(slike.rowEchelon(gen.to(vset)))
          def rowReducedEchelon(vset: V) = gen.from(slike.rowReducedEchelon(gen.to(vset)))

          def isZero(vset: V): Boolean = slike.isZero(gen.to(vset))
          def identity(size: Int): V = gen.from(slike.identity(size))
     }

     class GenericDimension[V, R](implicit gen: Generic.Aux[V, R], d: Dimension[R]) {
          //class GenericDimension[V, R](implicit gen: Generic.Aux[V, R], d: Dimension[R]) {
          def dimension(v: V): Int = d.dimension(gen.to(v))
     }

     class GenericEq[E, R](implicit gen: Generic.Aux[E, R], eq: Eq[R]) {
          def eqv(a: E, b: E): Boolean = eq.eqv(gen.to(a), gen.to(b))
     }


     // -------------------
     class MonoidForHList[H, T <: HList](implicit monoid: Lazy[Monoid[H]], defaultTail: DefaultInstance[T]){

          val zero: H :: T = monoid.value.zero  :: defaultTail.instance

          def plus(vset1: H :: T, vset2: H :: T): H :: T =
               monoid.value.plus(vset1.head, vset2.head) :: defaultTail.instance
     }

     class AbelianGroupForHList[H, T <: HList](implicit abelian: Lazy[AbelianGroup[H]],
                                               defaultTail: DefaultInstance[T]) extends MonoidForHList {

          def negate(vset: H :: T): H :: T = abelian.value.negate(vset.head) :: defaultTail.instance
     }

     class VectorSpaceForHList[H, T <: HList, F: Field](implicit vs: Lazy[VectorSpace[H, F]],
                                                        defaultTail: DefaultInstance[T]) extends AbelianGroupForHList {

          val one: H :: T = vs.value.one :: defaultTail.instance
          def scale(vset: H :: T, constant: F): H :: T = vs.value.scale(vset.head, constant) :: defaultTail.instance
     }

     //The way to generate an Amount type class instance for HLists so that the resulting typeclass
     // can be plugged into genericAmount[A, R] where R is the HList.
     class SetVecLikeForHList[H, T <: HList, F:Field](implicit slike: Lazy[SetVecLike[H, N]],
                                                        defaultTail: DefaultInstance[T]) extends
          VectorSpaceForHList[H, T, F] {
          /*implicit def setVecLikeForHList[H, T <: HList, N: Number](
                                                                        implicit slike: Lazy[SetVecLike[H, N]], defaultTail: DefaultInstance[T]
                                                                   ): SetVecLike[H :: T, N] = new SetVecLike[H :: T, N] {*/

          def minus(vset1: H :: T, vset2: H :: T): H :: T =
               slike.value.minus(vset1.head, vset2.head) :: defaultTail.instance

          def rowEchelon(vset: H :: T): H :: T = slike.value.rowEchelon(vset.head) :: defaultTail.instance
          def rowReducedEchelon(vset: H :: T): H :: T = slike.value.rowReducedEchelon(vset.head) :: defaultTail
               .instance
          def isZero(vset: H :: T): Boolean = slike.value.isZero(vset.head)
          def identity(size: Int): H :: T = slike.value.identity(size) :: defaultTail.instance
     }


     class DimensionForHList[H, T <: HList](implicit d: Lazy[Dimension[H]], defaultTail: DefaultInstance[T]){
          //class DimensionForHList[H, T <: HList](implicit d: Lazy[Dimension[H]], defaultTail: DefaultInstance[T]) {
          def dimension(v: H :: T): Int = d.value.dimension(v.head)
     }


     class EqForHList[H, T <: HList](implicit eq: Lazy[Eq[H]], defaultTail: DefaultInstance[T]) {
          def eqv(a: H :: T, b: H :: T): Boolean = eq.value.eqv(a.head, b.head)
     }


     // -----------



     trait SetVecIsMonoidHList extends Monoid[SetOfVectors[N] :: HNil] {
          val zero: SetOfVectors[N] :: HNil = SetOfVectors(Vector.ZERO[N](1)) :: HNil

          def plus(vset: SetOfVectors[N] :: HNil, wset: SetOfVectors[N] :: HNil): SetOfVectors[N] :: HNil ={
               Util.Gen.ensureSize(vset.head, wset.head)
               SetOfVectors(vset.head.getColumns().zip(wset.head.getColumns())
                    .map(colPair => colPair._1 + colPair._2):_*) :: HNil
          }
     }
     ////

     //TODO what happens if I uncomment the abelian of setvecs :: Hnil ???
     trait SetVecIsAbelianGroupHList extends SetVecIsMonoidHList with AbelianGroup[SetOfVectors[N] ::HNil]{

          def negate(vset: SetOfVectors[N] :: HNil): SetOfVectors[N] :: HNil =
               SetOfVectors(vset.head.getColumns().map(c => c.negate()):_*) :: HNil
     }


     trait SetVecIsVectorSpaceHList extends SetVecIsAbelianGroupHList with SetVecIsMonoidHList with VectorSpace[SetOfVectors[N] ::
          HNil, N] {

          val one: SetOfVectors[N] :: HNil = SetOfVectors(Vector.ONE[N](1)) :: HNil

          def scale(v: SetOfVectors[N] :: HNil, factor: N): SetOfVectors[N] :: HNil =
               SetOfVectors(v.head.getColumns().map(col => col.scale(factor)):_*) :: HNil

     }


     trait SetVecHasEqHList extends Eq[SetOfVectors[N] ::HNil] {
          def eqv(vset: SetOfVectors[N] :: HNil, wset: SetOfVectors[N] :: HNil): Boolean = {
               Util.Gen.ensureSize(vset.head, wset.head)

               vset.head.getColumns()
                    .zip(wset.head.getColumns())
                    .forall(colPair => Eq[Vector[N]].eqv(colPair._1, colPair._2))
          }
     }

     trait SetVecHasDimensionHList extends Dimension[SetOfVectors[N]::HNil] {
          //trait SetVecHasDimensionHList extends Dimension[SetOfVectors[N] ::HNil] {
          def dimension(vset: SetOfVectors[N] :: HNil): Int = vset.head.getColumns().head.dimension()
     }


     class SetVecIsSetVecLikeHList extends SetVecIsMonoidHList
          with SetVecIsAbelianGroupHList
          with SetVecIsVectorSpaceHList
          with SetVecHasDimensionHList
          with SetVecHasEqHList
          with SetVecLike[SetOfVectors[N] ::HNil, N] {
          //implicit def setVecForSetVecLikeHList[N: Number]: SetVecLike[SetOfVectors[N] :: HNil, N] = new
          //SetVecLike[SetOfVectors[N] :: HNil, N] {


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

     }


     //implicit val monoid = new SetVecIsMonoidHList
     //implicit val abelian = new SetVecIsAbelianGroupHList
     //implicit val vectorSpace = new SetVecIsVectorSpaceHList
     implicit val vsetLike = new SetVecIsSetVecLikeHList
     //implicit val dim = new SetVecHasDimensionHList
     //implicit val eq = new SetVecHasEqHList
}*/
package linalg.vector


import linalg.theory._
import linalg.theory.space._
import linalg.theory.basis._
import linalg.kernel._
import linalg.util.Util
import linalg.implicits._
import linalg.instances._
import cats.Eq
import linalg.show.Show

import scala.collection.mutable.{ListBuffer, Seq}
import scala.language.implicitConversions
import scala.language.higherKinds


/**
  * Features:
  *
  * //note:
  * - if there is a has-a relation, we use typeclass implementation.
  * If there is is-a relation use trait extension. Example: Vector class HAS-A Basis so declare the
  * Basis[Vector[N] ] implementation bu Vectr IS-A Hilbert Space so extend the Hilbert Space in VectorLike trait.
  *
  * -
  */

trait VectorLike[V, F] extends HilbertSpace[V, F] with NormedVectorSpace[V, F] {


     // inherited - plus, negate, scale, innerProduct, norm, angle
     def minus(v: V, w: V): V = plus(v, negate(w))
     def isZero(v: V): Boolean
     def crossProduct(v: V, w: V): Option[V]  //maybe won't work
     def outerProduct(v: V, w: V): SetOfVectors[F]

     def projection[R:RealNumber](v: V, onto: V)(implicit f: Field[F], r: NRoot[F,R]): V
     /*def get(v: V, i: Int): F
     def set(v: V, i: Int, value: F): Unit
     def toList(v: V): List[F]
     def toBuff(v: V): ListBuffer[F]*/
}


object VectorLike {


     implicit def VectorIsVectorLike[N: Number] = new VectorLike[Vector[N], N]
          with Dimension[Vector[N]] with Eq[Vector[N]] /*with Span[Vector[N], N]*/ {


          val zero: Vector[N] = Vector(Number.ZERO[N]) //just vector with one element
          val one: Vector[N] = Vector(Number.ONE[N]) //just vector with one element

          /** Dimension part */
          def dimension(v: Vector[N]): Int = v.getElements().length
               //todo decide later if can revert stuff in Number back to implicit def version like in here and just
               //todo while leaving the RootLike etc as add-on in Number Complex declaration - but sort of messy.

          /** Eq part */
          def eqv(v: Vector[N], w: Vector[N]): Boolean = v.getElements() == w.getElements()

          /** VectorLike part */
          def plus(v: Vector[N], w: Vector[N]): Vector[N] ={
               Util.Gen.ensureSize(v, w)
               Vector(v.getElements().zip(w.getElements()).map(pair => pair._1 + pair._2):_*)
          }

          def negate(v: Vector[N]): Vector[N] = Vector(v.getElements().map(e => e.negate()):_*)

          def scale(v: Vector[N], factor: N): Vector[N] = Vector(v.getElements().map(e => e * factor):_*)

          def isZero(v: Vector[N]): Boolean = v.getElements().forall(e => e :==: Number.ZERO[N])

          def projection[R:RealNumber](v: Vector[N], onto: Vector[N])(implicit field: Field[N],
                                                                      r: NRoot[N,R]): Vector[N] =
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

               if(dimension(u) == 3 && dimension(v) == 3){
                    val w1: N = (u.get(2) * v.get(3)) - (u.get(3) * v.get(2))
                    val w2: N = (u.get(3) * v.get(1)) - (u.get(1) * v.get(3))
                    val w3: N = (u.get(1) * v.get(2)) - (u.get(2) * v.get(1))

                    Some(Vector(w1, w2, w3))

               } else None
          }

          def angle[R:RealNumber](v: Vector[N], w: Vector[N])(implicit t: Trig[N],
                                                              field: Field[N], r: NRoot[N,R]): N =
               field.divide(innerProduct(v, w),  field.times(norm[R](v), norm[R](w)).arccos() )

          def norm[R:RealNumber](v: Vector[N])(implicit field: Field[N], root: NRoot[N, R]): N =
               Util.Gen.total[N](Vector(v.getElements().map(e => root.power(e, RealNumber.TWO[R])):_*))
          //goal: Util.Gen.total[N](v.map(e => ...))
          //todo test if this sums well, or if need explicit: .reduceLeft[N]((acc, y) => field.plus(acc,y))
     }
}





class Vector[N: Number](private val elems: N*) {

     private val elements: Seq[N] = Seq(elems:_*)

     def copy(): Vector[N] = Vector(elements:_*)
     def copy(es: Seq[N]): Vector[N] = Vector(es:_*)

     def set(index: Int)(value: N): Unit = elements(index) = value
     def get(index: Int): N = elements(index)
     def getElements(): Seq[N] = Seq(elements:_*)
     /*def toList: List[N] = elements.toList
     def toSeq: Seq[N] = Seq(elements:_*)*/

     //todo how does cats do it? Have Eq[A] not pink but a different blue color, value?
     override def toString: String = Show[Vector[N]].show(this)
}


object Vector {

     def apply[N: Number](elems: N*): Vector[N] = new Vector(elems:_*)
     def apply[N: Number](elems: Seq[N]): Vector[N] = new Vector(elems:_*)
     def apply[N: Number](elems: List[N]): Vector[N] = new Vector(elems:_*)
     def apply[N: Number](elems: ListBuffer[N]): Vector[N] = new Vector(elems:_*)

     //todo could go on, adding types, because if List < Seq but we do Vector(list), and list apply isn't there
     //todo then it doesn't see the seq apply and completely ignores it!

     def ZERO[N: Number](len: Int): Vector[N] = Vector(List.fill[N](len)(Number.ZERO[N]))
     def ONE[N: Number](len: Int): Vector[N] = Vector(Seq.fill[N](len)(Number.ONE[N]))



     //todo cannot because in compile time error - do not have implicit N:Number and M:Number, but can't add this to
     // functor map
     /*implicit val vectorFunctor = new Functor[Vector] {

          def map[N, M](fa: Vector[N])(f: N => M): Vector[M] ={
               Vector(fa.getElements().map(f):_*)
          }
     }

     implicit def vectorEq[N:Number] = new Eq[Vector[N]]{

          def eqv(v1: Vector[N], v2: Vector[N]): Boolean ={
               v1.getElements() == v2.getElements()
          }
     }

     implicit def vectorMonoid[N:Number](implicit monoidNum: cats.Monoid[N]) = new cats.Monoid[Vector[N]]{

          def empty: Vector[N] = Vector(monoidNum.empty)

          def combine(v: Vector[N], w: Vector[N]): Vector[N] ={
               Util.Gen.ensureSize(v, w)
               Vector(v.getElements().zip(w.getElements()).map(pair => pair._1 + pair._2):_*)
          }
     }*/
}







object VectorTester extends App {

     import VectorLike._

     val v1: Vector[Int] = Vector(1,2,3)
     val v2: Vector[Int] = Vector(2,0,4, 5)

     println(v1.negate())
     println(v1 + v2)
     println(Vector(2,3,4) + Vector(-2, 3, -6))
     println(v1.isZero)
     println(v1.dotProduct(v2))
     println(v1.norm())
     println(v1.isNormalized())
     println(v2.get(3))

}
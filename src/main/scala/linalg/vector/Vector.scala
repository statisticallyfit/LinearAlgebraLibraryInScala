package linalg.vector


//import linalg.theory._
//import linalg.theory.space._
//import linalg.kernel._
import linalg.implicits._
import linalg._
import linalg.instances._
//import linalg.util.Show

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

     def projection[R:RealNumber](v: V, onto: V)(implicit f: Field[F], r: RootLike[F,R]): V
     /*def get(v: V, i: Int): F
     def set(v: V, i: Int, value: F): Unit
     def toList(v: V): List[F]
     def toBuff(v: V): ListBuffer[F]*/
}
object VectorLike




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
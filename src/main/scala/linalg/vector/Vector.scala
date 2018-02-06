package linalg.vector


import linalg.numeric.{Number, Trig, Root}
import linalg.theory.space._
import linalg.theory._
import linalg.syntax.ShowSyntax._
import linalg.syntax.TrigSyntax._

import scala.language.implicitConversions
import scala.language.higherKinds

import spire.algebra.VectorSpace

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



trait VectorLike[V, F] extends InnerProductSpace[V, F] with HilbertSpace[V, F] with BanachSpace[V, F] {

     // inherited - plus, negate, scale, innerProduct
     def minus(v: V, w: V): V = plus(v, negate(w))
     def isZero(v: V): Boolean
     def crossProduct(v: V, w: V): V  //maybe won't work
     def outerProduct(v: V, w: V): V
}



object VectorLike {

     implicit def VectorIsVectorLike[N: Number: Trig](implicit root: Root[N,N]) = new VectorLike[Vector[N], N] {

          val zero: Vector[N] = Vector(Number.ZERO[N]) //just vector with one element
          val one: Vector[N] = Vector(Number.ONE[N]) //just vector with one element
          //implicit override val scalar: Field[N]


          import linalg.syntax.NumberSyntax._
          import Number._

          def plus(v: Vector[N], w: Vector[N]): Vector[N] =
               Vector(v.elems.zip(w.elems).map(pair => pair._1 + pair._2):_*)

          def negate(v: Vector[N]): Vector[N] = Vector(v.elems.map(e => e.negate()):_*)

          def scale(v: Vector[N], factor: N): Vector[N] = Vector(v.elems.map(e => e * factor):_*)

          def isZero(v: Vector[N]): Boolean = v.elems.forall(e => e == Number.ZERO[N])

          def innerProduct(v: Vector[N], w: Vector[N]): N =
               v.elems.zip(w.elems).map(pair => pair._1 * pair._2).reduceLeft((acc, y) => acc + y)

          def outerProduct(v: Vector[N], w: Vector[N]): Vector[N] = ??? //todo

          def crossProduct(v: Vector[N], w: Vector[N]): Vector[N] = ??? //todo

          def angle(v: Vector[N], w: Vector[N]): N = innerProduct(v, w) / (norm(v) * norm(w)).arccos()

          def norm(v: Vector[N]): N = v.elems.map(e => root.power(e, Number.TWO[N])).reduceLeft(_ + _)
     }
}


//
// ------------------------------------------------------------------------------------------------------------------------

class Vector[N: Number](val elems: N*){
     override def toString: String = Vector(elems:_*).show
}


object Vector {

     def ZERO[N: Number](len: Int): Vector[N] = Vector(List.fill[N](len)(Number.ZERO[N]):_*)
     def ONE[N: Number](len: Int): Vector[N] = Vector(List.fill[N](len)(Number.ONE[N]):_*)

     def apply[N: Number](elems: N*): Vector[N] = new Vector(elems:_*)
}


// ------------------------------------------------------------------------------------------------------------------------

class VectorSet[N: Number](val cols: Vector[N]*)


/*object VectorSet {

     //typeclasses ... etc

     implicit class VectorSetOps[V[_], N: Number](vset: VectorSet[N]){
          def reducedRowEchelonForm(): VectorSet[N] = ???
     }
}*/


// ------------------------------------------------------------------------------------------------------------------------

object VectorTester extends App {

     import linalg.syntax.VectorLikeSyntax._
     import VectorLike._


     //val vec = implicitly[VectorLike[Vector[Int], Int]]

     val v1: Vector[Int] = Vector.ONE[Int](10)
     val v2: Vector[Int] = Vector.ONE[Int](10)
     val v3: Vector[Int] = v1 + v2

     //println(vec.plus(v1, v2))
     //println(v3)


}
package linalg.vector

//TODO tomorrow - no self-types, just do general approach like in Number class.

import linalg.numeric._
import linalg.theory.space._
import linalg.syntax.AbsoluteSyntax._
import linalg.syntax.EqualSyntax._
import linalg.syntax.NumberSyntax._
import linalg.syntax.RootSyntax._
import linalg.syntax.ShowSyntax._
import linalg.syntax.TrigSyntax._
import linalg.syntax.VectorLikeSyntax._

import scala.language.implicitConversions
import scala.language.higherKinds

//todo: then make trait MatrixLike[M] and implement adjoint, ... all matrix-type stuff.
//make square matrix and the rest do the same. or inherit from matrix???
//todo then Independence[L] and Basis[B] traits to have functions basis() ...- the typeclass way.

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
//todo use Cats Eq typeclass instead!


trait VectorLike[V, F] extends InnerProductSpace[V, F] with HilbertSpace[V, F] with BanachSpace[V, F] {

     //note: inherited - plus, negate, scale, innerProduct
     def minus(v: V, w: V): V = plus(v, negate(w))
     def isZero(v: V): Boolean
     def crossProduct(v: V, w: V): V  //maybe won't work
     def outerProduct(v: V, w: V): V
}


class Vector[N: Number](val elems: N*){
     override def toString: String = Vector(elems:_*).show
}

object Vector {

     def ZERO[N: Number](len: Int): Vector[N] = Vector(List.fill[N](len)(Number.ZERO[N]):_*)
     def ONE[N: Number](len: Int): Vector[N] = Vector(List.fill[N](len)(Number.ONE[N]):_*)

     def apply[N: Number](elems: N*): Vector[N] = new Vector(elems:_*)







     implicit def VectorIsVectorLike[N: Number: Trig](implicit root: Root[N,N]) = new VectorLike[Vector[N], N] {

          import Number._ //for implicit numberops syntax


          val zero: Vector[N] = Vector(Number.ZERO[N]) //just vector with one element
          val one: Vector[N] = Vector(Number.ONE[N]) //just vector with one element


          def plus(v: Vector[N], w: Vector[N]): Vector[N] =
               Vector(v.elems.zip(w.elems).map(pair => pair._1 + pair._2):_*)

          def negate(v: Vector[N]): Vector[N] = Vector(v.elems.map(e => e.negate()):_*)

          def scale(v: Vector[N], factor: N): Vector[N] = Vector(v.elems.map(e => e * factor):_*)

          def isZero(v: Vector[N]): Boolean = v.elems.forall(e => e == Number.ZERO[N])

          def innerProduct(v: Vector[N], w: Vector[N]): N =
               v.elems.zip(w.elems).map(pair => pair._1 * pair._2).reduceLeft((acc, y) => acc + y)

          def outerProduct(v: Vector[N], w: Vector[N]): Vector[N] = ???

          def crossProduct(v: Vector[N], w: Vector[N]): Vector[N] = ???

          def angle(v: Vector[N], w: Vector[N]): N = innerProduct(v, w) / (norm(v) * norm(w)).arccos()

          def norm(v: Vector[N]): N = v.elems.map(e => root.power(e, Number.TWO[N])).reduceLeft(_ + _)
     }


}


object VectorTester extends App {

     import Vector._

     val vec = implicitly[VectorLike[Vector[Int], Int]]

     val v1: Vector[Int] = Vector.ONE[Int](10)
     val v2: Vector[Int] = Vector.ONE[Int](10)
     //val v3: Vector[Int] = v1 + v2

     println(vec.plus(v1, v2))
     //println(v3)


}
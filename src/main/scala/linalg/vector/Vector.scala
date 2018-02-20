package linalg.vector


import linalg.implicits._
import linalg.kernel._
import linalg.theory.space._
import linalg.theory._
import linalg.theory.basis.Dimension

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


class Vector[N: Number](private val elems: N*) {

     private val elements: Seq[N] = Seq(elems:_*)

     def copy(): Vector[N] = Vector(elements:_*)
     def copy(es: Seq[N]): Vector[N] = Vector(es:_*)

     def set(index: Int)(value: N): Unit = elements(index) = value
     def get(index: Int): N = elements(index)
     def getElements(): Seq[N] = Seq(elements:_*)

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
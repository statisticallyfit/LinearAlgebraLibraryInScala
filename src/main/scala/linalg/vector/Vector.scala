package linalg.vector


import linalg.numeric.{Number, Root0, Trig, Compare}
import linalg.theory.space._
import linalg.theory._
import linalg.syntax.TrigSyntax._
import linalg.syntax.ShowSyntax._
import linalg.syntax.CompareSyntax._
//import linalg.syntax.NumberSyntax._

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


trait VectorLike[V, F]
     extends InnerProductSpace[V, F]
     with HilbertSpace[V, F]
     with NormedVectorSpace[V, F] {

     // inherited - plus, negate, scale, innerProduct, norm, angle
     def minus(v: V, w: V): V = plus(v, negate(w))
     def isZero(v: V): Boolean
     def crossProduct(v: V, w: V): SetOfVectors[F]  //maybe won't work
     def outerProduct(v: V, w: V): SetOfVectors[F]
}



object VectorLike {

     //NOTE: use root0 not root because the N might be a complex

     implicit def VectorIsVectorLike[N: Number: Trig: Compare](implicit root: Root0[N,N]) = new
               VectorLike[Vector[N], N] {

          import linalg.syntax.NumberSyntax._


          val zero: Vector[N] = Vector(Number.ZERO[N]) //just vector with one element
          val one: Vector[N] = Vector(Number.ONE[N]) //just vector with one element


          def plus(v: Vector[N], w: Vector[N]): Vector[N] =
               Vector(v.elements.zip(w.elements).map(pair => pair._1 + pair._2):_*)

          def negate(v: Vector[N]): Vector[N] = Vector(v.elements.map(e => e.negate()):_*)

          def scale(v: Vector[N], factor: N): Vector[N] = Vector(v.elements.map(e => e * factor):_*)

          def isZero(v: Vector[N]): Boolean = v.elements.forall(e => e :==: Number.ZERO[N])

          def innerProduct(v: Vector[N], w: Vector[N]): N =
               v.elements.zip(w.elements).map(pair => pair._1 * pair._2).reduceLeft((acc, y) => acc + y)

          def outerProduct(v: Vector[N], w: Vector[N]): SetOfVectors[N] = ??? //todo

          def crossProduct(v: Vector[N], w: Vector[N]): SetOfVectors[N] = ??? //todo

          def angle(v: Vector[N], w: Vector[N]): N = innerProduct(v, w) / (norm(v) * norm(w)).arccos()

          def norm(v: Vector[N])(implicit div: Field[N]): N =
               v.elements.map(e => root.power(e, Number.TWO[N])).reduceLeft(_ + _)
     }
}





class Vector[N: Number](val elements: N*){

     override def toString: String = Vector(elements:_*).show
}


object Vector {

     def ZERO[N: Number](len: Int): Vector[N] = Vector(List.fill[N](len)(Number.ZERO[N]):_*)
     def ONE[N: Number](len: Int): Vector[N] = Vector(List.fill[N](len)(Number.ONE[N]):_*)

     def apply[N: Number](elems: N*): Vector[N] = new Vector(elems:_*)
}







object VectorTester extends App {

     import linalg.numeric.Number._
     import linalg.syntax.VectorLikeSyntax._



     val v1: Vector[Int] = Vector(1,2,3)
     val v2: Vector[Int] = Vector(2,0,4)

     println(v1.negate())
     println(v1 + v2)
     println(Vector(2,3,4) + Vector(-2, 3, -6))
     println(v1.isZero())
     println(v1.dotProduct(v2))

}
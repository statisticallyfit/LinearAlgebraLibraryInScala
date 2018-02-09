
import linalg.numeric._
import linalg.theory.space._
import linalg.syntax.CompareSyntax._
import linalg.syntax.TrigSyntax._
import linalg.theory._


trait VectorLike[V, F]
     extends InnerProductSpace[V, F]
          with HilbertSpace[V, F]
          with NormedVectorSpace[V, F] {

     // inherited - plus, negate, scale, innerProduct, norm, angle
     def minus(v: V, w: V): V = plus(v, negate(w))
     def isZero(v: V): Boolean
     //def crossProduct(v: V, w: V): V  //maybe won't work
     //def outerProduct(v: V, w: V): V
}


object VectorLike {

     //NOTE: use root0 not root because the N might be a complex


     implicit class VectorLikeOps[V, N: Number: Trig](current: V)
                                                     (implicit root: Root0[N, N],
                                                      vectorLike: VectorLike[V, N]){

          def isZero(): Boolean = vectorLike.isZero(current)
          def +(other: V): V = vectorLike.plus(current, other)
          def add(other: V): V = vectorLike.plus(current, other)
          def -(other: V): V = vectorLike.minus(current, other)
          def negate(): V = vectorLike.negate(current)
          def scale(factor: N): V = vectorLike.scale(current, factor)

          def norm(): N = vectorLike.norm(current)
          def angle(other: V): N = vectorLike.angle(current, other)
          def innerProduct(other: V): N = vectorLike.innerProduct(current, other)
          def dotProduct(other: V): N = vectorLike.dotProduct(current, other)

     }


     implicit def VectorIsVectorLike[N: Number: Trig: Compare](implicit root: Root0[N,N]) = new
               VectorLike[Vector[N],  N] {

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

          def angle(v: Vector[N], w: Vector[N]): N = innerProduct(v, w) / (norm(v) * norm(w)).arccos()

          def norm(v: Vector[N])(implicit div: Field[N]): N =
               v.elements.map(e => root.power(e, Number.TWO[N])).reduceLeft(_ + _)
     }

}


//
// ------------------------------------------------------------------------------------------------------------------------

class Vector[N: Number](val elements: N*)

object Vector {

     def ZERO[N: Number](len: Int): Vector[N] = Vector(List.fill[N](len)(Number.ZERO[N]):_*)
     def ONE[N: Number](len: Int): Vector[N] = Vector(List.fill[N](len)(Number.ONE[N]):_*)

     def apply[N: Number](elems: N*): Vector[N] = new Vector(elems:_*)
}

import VectorLike._

val v1: VectorLike[Vector[Int], Int] = Vector(1,2,3).asInstanceOf[VectorLike[Vector[Int],Int]]
val a = new VectorLikeOps(v1).add(v1)

println(a)





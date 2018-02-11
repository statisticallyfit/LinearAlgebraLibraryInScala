package linalg.vector


import linalg.theory._
import linalg.theory.space._
import linalg.theory.basis._
import linalg.syntax.VectorLikeSyntax._
import linalg.numeric._ //{Number, Trig, Compare, Root0, Root}
import linalg.numeric.Number._
import linalg.syntax.RootSyntax._
import linalg.syntax.NumberSyntax._
import linalg.syntax.TrigSyntax._
import linalg.syntax.ShowSyntax._
import linalg.syntax.CompareSyntax._

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

     //def size(v: V)(implicit d: Dimension[V]): Int //todo replace with Dimension trait result
}

/*trait SizeChecker[V] {
     def ensureSameSize(v: V, w: V): Boolean
}*/


object VectorLike {

     /*implicit def VectorLikeHasSameSize[V[_], N](implicit vecLike: VectorLike[V[N], N]) = new SizeChecker[V[N]]{

          def ensureSameSize(v: V[N], w: V[N]): Boolean = vecLike.size(v) == vecLike.size(w)
     }*/
     //NOTE: use root0 not root because the N might be a complex

     //(implicit ensure: SizeChecker[Vector[N]])
     implicit def VectorIsVectorLike[N: Number: Trig: Compare: Root: Absolute] = new VectorLike[Vector[N], N] {

          implicit val vectorSpaceHasDimension: Dimension[Vector[N]] = new Dimension[Vector[N]] {
               def dimension(v: Vector[N]): Int = v.elements.length
          }

          val zero: Vector[N] = Vector(Number.ZERO[N]) //just vector with one element
          val one: Vector[N] = Vector(Number.ONE[N]) //just vector with one element
          //val ensureSize = implicitly[SizeChecker[Vector[N]]]

          //todo how to ensure they are the same size? use implicits? how?

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

          def norm(v: Vector[N])(implicit f: Field[N]): N =
               v.elements.map(e => e ^ Number.TWO[N]).reduceLeft(_ + _)

          def size(v: Vector[N]): Int = v.elements.length

          /*private def innerProdRealLike(v: Vector[R], w: Vector[R]): R =
               v.elements.zip(w.elements).map(pair => pair._1 * pair._2).reduceLeft((acc, y) => acc + y)

          def angle(v: Vector[R], w: Vector[R]): R = innerProdRealLike(v, w) / (norm(v) * norm(w)).arccos()

          def norm(v: Vector[R])(implicit div: Field[R]): R =
               v.elements.map(e => e ^ Number.TWO[R]).reduceLeft(_ + _)*/
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


     val v1: Vector[Int] = Vector(1,2,3)
     val v2: Vector[Int] = Vector(2,0,4, 5)

     println(v1.negate())
     println(v1 + v2)
     println(Vector(2,3,4) + Vector(-2, 3, -6))
     println(v1.isZero())
     println(v1.dotProduct(v2))

}
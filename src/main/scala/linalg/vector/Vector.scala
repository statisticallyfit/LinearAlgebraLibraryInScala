package linalg.vector


import linalg.theory._
import linalg.theory.space._
import linalg.theory.basis._
import linalg.numeric._ //{Number, Trig, Compare, Root0, Root}
import linalg.numeric.Number._
import linalg.syntax.NumberSyntax._
import linalg.syntax.RootSyntax._
import linalg.syntax.TrigSyntax._
import linalg.syntax.ShowSyntax._
import linalg.syntax.CompareSyntax._
import linalg.syntax.VectorLikeSyntax._
import linalg.syntax.VectorSpaceSyntax._
import linalg.util.Exception._
import linalg.util.Util.SizeChecker

import cats.Eq

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

     def get(v: V, i: Int): F
}


object VectorLike {

     implicit def VectorIsVectorLike[N: Number: Trig: Compare: Root: Absolute] = new VectorLike[Vector[N], N]
          with Dimension[Vector[N]] with Eq[Vector[N]] with SizeChecker[Vector[N]] /*with Basis[Vector[N], N]*/ {

          /*implicit val vectorSpaceHasDimension: Dimension[Vector[N]] = new Dimension[Vector[N]] {
               def dimension(v: Vector[N]): Int = v.elements.length
          }*/
          val zero: Vector[N] = Vector(Number.ZERO[N]) //just vector with one element
          val one: Vector[N] = Vector(Number.ONE[N]) //just vector with one element

          def ensureSize(v: Vector[N], w: Vector[N], SIZE: Int = 0): Unit = {

               val caseVectorsAreDifferentSize: Boolean = (SIZE == 0 || SIZE < 0) && (dimension(v) != dimension(w))
               val caseVectorsAreDifferentThanSpecificSize: Boolean = SIZE != dimension(v) || SIZE != dimension(w)

               if(caseVectorsAreDifferentSize || caseVectorsAreDifferentThanSpecificSize){
                    throw VectorLikeSizeException("Vectors are not same size; cannot continue operation.")
               }
          }

          /** Eq part */
          def eqv(v: Vector[N], w: Vector[N]): Boolean = v.elements == w.elements

          /** VectorLike part */
          def plus(v: Vector[N], w: Vector[N]): Vector[N] ={
               ensureSize(v, w)
               Vector(v.elements.zip(w.elements).map(pair => pair._1 + pair._2):_*)
          }

          def negate(v: Vector[N]): Vector[N] = Vector(v.elements.map(e => e.negate()):_*)

          def scale(v: Vector[N], factor: N): Vector[N] = Vector(v.elements.map(e => e * factor):_*)

          def isZero(v: Vector[N]): Boolean = v.elements.forall(e => e :==: Number.ZERO[N])

          def outerProduct(v: Vector[N], w: Vector[N]): SetOfVectors[N] = {
               ensureSize(v, w)

               val as: Seq[N] = v.elements
               val bs: Seq[N] = w.elements

               val result: Seq[Seq[N]] = as.map(a => bs.map(b => a * b))

               SetOfVectors.fromSequences(result:_*)
          }

          def innerProduct(v: Vector[N], w: Vector[N]): N = {
               ensureSize(v, w)
               v.elements.zip(w.elements).map(pair => pair._1 * pair._2).reduceLeft((acc, y) => acc + y)
          }

          def crossProduct(u: Vector[N], v: Vector[N]): Option[Vector[N]] = {

               if(dimension(u) == 3 && dimension(v) == 3){
                    val w1: N = (get(u,2) * get(v,3)) - (get(u,3) * get(v,2))
                    val w2: N = (get(u,3) * get(v,1)) - (get(u,1) * get(v,3))
                    val w3: N = (get(u,1) * get(v,2)) - (get(u,2) * get(v,1))

                    Some(Vector(w1, w2, w3))

               } else None
          }

          def angle(v: Vector[N], w: Vector[N]): N = innerProduct(v, w) / (norm(v) * norm(w)).arccos()

          def norm(v: Vector[N])(implicit f: Field[N]): N =
               v.elements.map(e => e ^ Number.TWO[N]).reduceLeft(_ + _)

          def dimension(v: Vector[N]): Int = v.elements.length

          def get(v: Vector[N], i: Int): N = {
               val elements = v.elements.toList
               elements(i)
          }
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

     import VectorLike._

     val v1: Vector[Int] = Vector(1,2,3)
     val v2: Vector[Int] = Vector(2,0,4, 5)

     println(v1.negate())
     println(v1 + v2)
     println(Vector(2,3,4) + Vector(-2, 3, -6))
     println(v1.isZero())
     println(v1.dotProduct(v2))
     println(v1.norm())
     println(v1.isNormalized())

}
package linalg.vector


import linalg.numeric._ //{Number, Root0, Trig}
import linalg.theory.space._
import linalg.theory._
import linalg.syntax.TrigSyntax._
import linalg.syntax.ShowSyntax._
import linalg.syntax.EquivSyntax._

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


trait VectorLike[V, F] extends InnerProductSpace[V, F] with HilbertSpace[V, F] with NormedVectorSpace[V, F] {

     // inherited - plus, negate, scale, innerProduct
     def minus(v: V, w: V): V = plus(v, negate(w))
     def isZero(v: V): Boolean
     def crossProduct(v: V, w: V): V  //maybe won't work
     def outerProduct(v: V, w: V): V
}



object VectorLike {

     //NOTE: use root0 not root because the N might be a complex

     implicit def VectorIsVectorLike[N: Number: Trig: Equiv](implicit root: Root0[N,N]) = new
               VectorLike[Vector[N], N] {

          import linalg.syntax.NumberSyntax._


          val zero: Vector[N] = Vector(Number.ZERO[N]) //just vector with one element
          val one: Vector[N] = Vector(Number.ONE[N]) //just vector with one element


          def plus(v: Vector[N], w: Vector[N]): Vector[N] =
               Vector(v.elems.zip(w.elems).map(pair => pair._1 + pair._2):_*)

          def negate(v: Vector[N]): Vector[N] = Vector(v.elems.map(e => e.negate()):_*)

          def scale(v: Vector[N], factor: N): Vector[N] = Vector(v.elems.map(e => e * factor):_*)

          def isZero(v: Vector[N]): Boolean = v.elems.forall(e => e :==: Number.ZERO[N])

          def innerProduct(v: Vector[N], w: Vector[N]): N =
               v.elems.zip(w.elems).map(pair => pair._1 * pair._2).reduceLeft((acc, y) => acc + y)

          def outerProduct(v: Vector[N], w: Vector[N]): Vector[N] = ??? //todo

          def crossProduct(v: Vector[N], w: Vector[N]): Vector[N] = ??? //todo

          def angle(v: Vector[N], w: Vector[N]): N = innerProduct(v, w) / (norm(v) * norm(w)).arccos()

          def norm(v: Vector[N])(implicit div: Field[N]): N =
               v.elems.map(e => root.power(e, Number.TWO[N])).reduceLeft(_ + _)
     }


     implicit def VectorSetIsVectorLike[N: Number: Trig: Equiv](implicit vector: VectorLike[Vector[N], N]) = new
               VectorLike[SetOfVectors[N], N]{

          import linalg.syntax.NumberSyntax._
          import linalg.syntax.VectorLikeSyntax._

          val zero: SetOfVectors[N] = SetOfVectors(Vector.ZERO[N](1))
          val one: SetOfVectors[N] = SetOfVectors(Vector.ONE[N](1))

          def plus(v: SetOfVectors[N], w: SetOfVectors[N]): SetOfVectors[N] =
               SetOfVectors(v.cols.zip(w.cols).map(pair => pair._1 + pair._2):_*) //vector.plus(pair._1, pair._2)
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

class SetOfVectors[N: Number](val cols: Vector[N]*)


object SetOfVectors {

     //typeclasses ... etc

     def apply[N: Number](cols: Vector[N]*): SetOfVectors[N] = SetOfVectors(cols:_*)
}


// ------------------------------------------------------------------------------------------------------------------------

object VectorTester extends App {

     import linalg.syntax.VectorLikeSyntax._
     import VectorLike._



     val v1: Vector[Int] = Vector(1,2,3)
     val v2: Vector[Int] = Vector(2,0,4)
     /*val v3: Vector[Int] = v1.add(v1)
     v1 + v2*/

     //     v1 ~ v2
     //     v1.isZero()


     //println(Vector(1,2).innerProduct(v2))
     //println(vec.plus(v1, v2))
     //println(v3)

     //-------
     //note: this did not work either.
     /*trait Tester[W, E] {
          def testingMethod(x: W, y: E): String
     }

     import linalg.numeric.{RealLike, Complex}

     implicit class testerops[W, E: RealLike](currentComplex: W)(implicit w: Tester[W, E]){
          def testing(evalue: E): String = w.testingMethod(currentComplex, evalue)
          def plus(other: W): String = s"$currentComplex paired (+) with $other"
     }

     implicit def ComplexIsTester[R: RealLike](implicit root: Root[R,R]): Tester[Complex[R], R] = new Tester[Complex[R], R] {
          def testingMethod(x: Complex[R], real: R): String = s"$x with real $real"
     }*/

     /*val comp: Complex[Int] = new Complex(3, 2)
     comp.plus(comp) */
}
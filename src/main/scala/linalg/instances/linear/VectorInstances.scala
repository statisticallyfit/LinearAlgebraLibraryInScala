package linalg.instances.linear


import spire.algebra.Eq
import linalg.implicits._
import linalg._
import linalg.util._
import linalg.vector.{Polynomial, SetOfVectors, Vector}

import scala.collection.mutable.Seq

/**
  *
  */

class VectorThings[N: Number]{

     //Problem no implicit value since Number does not inherit from Absolute[N,N] only
     //RealNumber does that, same with Root

     /*class VectorHasAbsoluteValue extends Absolute[Vector[N], Vector[N]]{
          def absoluteValue(v: Vector[N]): Vector[N] =
               Vector(v.getElements().map(e => Absolute[N, N].absoluteValue(e)):_*)
     }*/

     class VectorIsMonoid extends Monoid[Vector[N]]{

          val zero: Vector[N] = Vector(Number.ZERO[N]) //just vector with one element
          def plus(v: Vector[N], w: Vector[N]): Vector[N] = Util.plus(v, w)
     }

     class VectorIsAbelianGroup extends VectorIsMonoid with AbelianGroup[Vector[N]]{
          def negate(v: Vector[N]): Vector[N] = Util.negate(v)
     }

     class VectorIsVectorSpace extends VectorIsAbelianGroup with VectorSpace[Vector[N], N]{

          val one: Vector[N] = Vector(Number.ONE[N]) //just vector with one element
          def scale(v: Vector[N], factor: N): Vector[N] = Util.scale(v, factor)
     }

     class VectorIsInnerProductSpace extends VectorIsVectorSpace with InnerProductSpace[Vector[N], N]{
          def innerProduct(v: Vector[N], w: Vector[N]): N = Util.innerProduct(v, w)
     }

     class VectorIsNormedVectorSpace extends VectorIsInnerProductSpace with NormedVectorSpace[Vector[N], N]{
          def norm(v: Vector[N])(implicit field: Field[N], root: Root[N, N]): N = Util.norm(v)
     }

     class VectorIsHilbertSpace extends VectorIsNormedVectorSpace with HilbertSpace[Vector[N], N]{
          def angle(v: Vector[N], w: Vector[N])(implicit t: Trig[N], field: Field[N], r: Root[N,N]): N =
               Util.angle(v, w)
     }

     class VectorIsVectorLike extends VectorIsHilbertSpace with VectorLike[Vector[N], N]{

          def isZero(v: Vector[N]): Boolean = Util.isZero(v)
          def projection(v: Vector[N], onto: Vector[N])(implicit field: Field[N], r: Root[N,N]): Vector[N] =
               Util.projection(v, onto)
          def outerProduct(v: Vector[N], w: Vector[N]): SetOfVectors[N] = Util.outerProduct(v, w)
          def crossProduct(u: Vector[N], v: Vector[N]): Option[Vector[N]] = Util.crossProduct(u, v)
          def size(v: Vector[N]): Int = v.getElements().length
          def transpose(v: Vector[N]): Vector[N] = {
               if(v.isRow()){
                    v.toCol()
                    v
               } else {
                    v.toRow()
                    v
               }
          }
     }

     /*class VectorHasDimension extends Dimension[Vector[N]] {
          def dimension(v: Vector[N]): Int = 1 //dim of vec = num of vectors in a basis for the vec
     }*/

     class VectorHasEq extends Eq[Vector[N]] {
          def eqv(v: Vector[N], w: Vector[N]): Boolean = Util.eqv(v, w)
     }

     class VectorCanHaveLinearIndependence extends LinearIndependence[Vector[N]] {

          def linearlyIndependent(v: Vector[N], w: Vector[N]): Boolean = Util.linearlyIndependent(v, w)
          def isLinearlyIndependent(v: Vector[N]): Boolean = Util.isLinearlyIndependent(v)
     }

     //val absolute = new VectorHasAbsoluteValue
     val eq = new VectorHasEq
     val monoid = new VectorIsMonoid
     val abelian = new VectorIsAbelianGroup
     val vectorSpace = new VectorIsVectorSpace
     val innerSpace = new VectorIsInnerProductSpace
     val normedSpace = new VectorIsNormedVectorSpace
     val hilbertSpace = new VectorIsHilbertSpace
     val vectorLike = new VectorIsVectorLike
     //val dim = new VectorHasDimension
     val independence = new VectorCanHaveLinearIndependence
}


trait VectorInstances {
     //implicit final def vectorHasAbsoluteValue[N: Number] = new VectorThings[N].absolute
     implicit final def vectorIsMonoid[N: Number] = new VectorThings[N].monoid
     implicit final def vectorIsAbelianGroup[N: Number] = new VectorThings[N].abelian
     implicit final def vectorIsVectorSpace[N: Number] = new VectorThings[N].vectorSpace
     implicit final def vectorIsInnerProductSpace[N: Number] = new VectorThings[N].innerSpace
     implicit final def vectorIsNormedVectorSpace[N: Number] = new VectorThings[N].normedSpace
     implicit final def vectorIsHilbertSpace[N: Number] = new VectorThings[N].hilbertSpace
     implicit final def vectorIsLikeAVector[N: Number] = new VectorThings[N].vectorLike
     //implicit final def vectorHasDimension[N: Number] = new VectorThings[N].dim
     implicit final def vectorHasEq[N: Number] = new VectorThings[N].eq
     implicit final def vectorCanBeLinearlyIndependent[N: Number] = new VectorThings[N].independence
}


package linalg.instances.linear


import spire.algebra.Eq

import linalg.implicits._
import linalg._
import linalg.util._
import linalg.vector.{SetOfVectors, Vector}

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

          def plus(v: Vector[N], w: Vector[N]): Vector[N] ={
               Util.Gen.ensureSize(v, w)
               Vector(v.getElements().zip(w.getElements()).map(pair => pair._1 + pair._2):_*)
          }
     }


     class VectorIsAbelianGroup extends VectorIsMonoid with AbelianGroup[Vector[N]]{

          def negate(v: Vector[N]): Vector[N] = Vector(v.getElements().map(e => e.negate()):_*)
     }


     class VectorIsVectorSpace extends VectorIsAbelianGroup with VectorSpace[Vector[N], N]{

          val one: Vector[N] = Vector(Number.ONE[N]) //just vector with one element

          def scale(v: Vector[N], factor: N): Vector[N] = Vector(v.getElements().map(e => e * factor):_*)

     }

     class VectorIsInnerProductSpace extends VectorIsVectorSpace with InnerProductSpace[Vector[N], N]{

          def innerProduct(v: Vector[N], w: Vector[N]): N = {
               Util.Gen.ensureSize(v, w)
               v.getElements().zip(w.getElements()).map(pair => pair._1 * pair._2).reduceLeft((acc, y) => acc + y)
          }
     }


     class VectorIsNormedVectorSpace extends VectorIsInnerProductSpace with NormedVectorSpace[Vector[N], N]{

          def norm(v: Vector[N])(implicit field: Field[N], root: Root[N, N]): N = {

               val two: N = field.one + field.one
               val sum: N = Util.Gen.sumElements[N](Vector(v.getElements().map(e => root.power(e, two)):_*))
               sum.sqrt()
          }
     }


     class VectorIsHilbertSpace extends VectorIsNormedVectorSpace with HilbertSpace[Vector[N], N]{

          def angle(v: Vector[N], w: Vector[N])(implicit t: Trig[N], field: Field[N], r: Root[N,N]): N =
               field.divide(innerProduct(v, w),  field.times(norm(v), norm(w)).arccos() )
     }


     class VectorIsVectorLike extends VectorIsHilbertSpace with VectorLike[Vector[N], N]{

          def isZero(v: Vector[N]): Boolean = v.getElements().forall(e => e.isZero)

          def projection(v: Vector[N], onto: Vector[N])(implicit field: Field[N], r: Root[N,N]): Vector[N] =
               scale(onto, field.divide(innerProduct(v, onto), norm(onto)) )

          def outerProduct(v: Vector[N], w: Vector[N]): SetOfVectors[N] = {
               Util.Gen.ensureSize(v, w)

               val as: Seq[N] = v.getElements()
               val bs: Seq[N] = w.getElements()

               val result: Seq[Seq[N]] = as.map(a => bs.map(b => a * b))

               SetOfVectors.fromSeqs(result:_*)
          }

          def crossProduct(u: Vector[N], v: Vector[N])/*(implicit d: Dimension[Vector[N]])*/:
          Option[Vector[N]] = {

               //TODO hacky way to get dimension/length but with Dimension doesn't work...
               if(u.getElements().length == 3 && v.getElements().length == 3){
                    val w1: N = (u.get(2) * v.get(3)) - (u.get(3) * v.get(2))
                    val w2: N = (u.get(3) * v.get(1)) - (u.get(1) * v.get(3))
                    val w3: N = (u.get(1) * v.get(2)) - (u.get(2) * v.get(1))

                    Some(Vector(w1, w2, w3))

               } else None
          }
     }


     class VectorHasDimension extends Dimension[Vector[N]] {
          def dimension(v: Vector[N]): Int = v.getElements().length
     }

     class VectorHasEq extends Eq[Vector[N]] {

          def eqv(v: Vector[N], w: Vector[N]): Boolean ={
               v.getElements().zip(w.getElements())
                    .forall(vwElemPair => vwElemPair._1 :==: vwElemPair._2)
          }
     }

     class VectorCanHaveLinearIndependence extends LinearIndependence[Vector[N]] {

          def linearlyIndependent(v: Vector[N], w: Vector[N]): Boolean ={
               val rref: SetOfVectors[N] = Util.Gen.rowReducedEchelon[N](SetOfVectors(v, w))
               val rrefSquare: SetOfVectors[N] = SetOfVectors(Util.Gen.expressRowsAsCols[N](rref.getRowsAt(0, 1)):_*)
               //rref :==: SetOfVectors.IDENTITY[N](2)
               ???
          }

          def isLinearlyIndependent(v: Vector[N]): Boolean = true //yes a single vector is lin indep.
     }

     //val absolute = new VectorHasAbsoluteValue
     val monoid = new VectorIsMonoid
     val abelian = new VectorIsAbelianGroup
     val vectorSpace = new VectorIsVectorSpace
     val innerSpace = new VectorIsInnerProductSpace
     val normedSpace = new VectorIsNormedVectorSpace
     val hilbertSpace = new VectorIsHilbertSpace
     val vectorLike = new VectorIsVectorLike
     val dim = new VectorHasDimension
     val eq = new VectorHasEq
     val linearIndep = new VectorCanHaveLinearIndependence
}


trait VectorInstances {

     //TODO test whether not strictly necessary to have each one like this, can just have
     //the ending trait VectorLike as class and instance below like in ComplexIsNumber ...

     //implicit final def vectorHasAbsoluteValue[N: Number] = new VectorThings[N].absolute
     implicit final def vectorIsMonoid[N: Number] = new VectorThings[N].monoid
     implicit final def vectorIsAbelianGroup[N: Number] = new VectorThings[N].abelian
     implicit final def vectorIsVectorSpace[N: Number] = new VectorThings[N].vectorSpace
     implicit final def vectorIsInnerProductSpace[N: Number] = new VectorThings[N].innerSpace
     implicit final def vectorIsNormedVectorSpace[N: Number] = new VectorThings[N].normedSpace
     implicit final def vectorIsHilbertSpace[N: Number] = new VectorThings[N].hilbertSpace
     implicit final def vectorIsLikeAVector[N: Number] = new VectorThings[N].vectorLike
     implicit final def vectorHasDimension[N: Number] = new VectorThings[N].dim
     implicit final def vectorHasEq[N: Number] = new VectorThings[N].eq
     implicit final def vectorCanBeLinearlyIndependent[N: Number] = new VectorThings[N].linearIndep
}


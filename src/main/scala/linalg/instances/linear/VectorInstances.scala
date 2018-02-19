package linalg.instances.linear

import linalg.implicits._
import linalg.kernel.{Number, RealNumber, Root, Trig}
import linalg.theory.basis.Dimension
import linalg.theory.{AbelianGroup, Field, Monoid}
import linalg.theory.space.{HilbertSpace, InnerProductSpace, NormedVectorSpace, VectorSpace}
import linalg.util.Util
import linalg.vector.{SetOfVectors, Vector, VectorLike}

import scala.collection.mutable.Seq

/**
  *
  */

class VectorInstances[N: Number]{

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

          def norm[R:RealNumber](v: Vector[N])(implicit field: Field[N], root: Root[N, R]): N =
               Util.Gen.total[N](Vector(v.getElements().map(e => root.power(e, RealNumber[R].two)):_*))
     }


     class VectorIsHilbertSpace extends VectorIsNormedVectorSpace with HilbertSpace[Vector[N], N]{

          def angle[R:RealNumber](v: Vector[N], w: Vector[N])(implicit t: Trig[N],
                                                              field: Field[N], r: Root[N,R]): N =
               field.divide(innerProduct(v, w),  field.times(norm[R](v), norm[R](w)).arccos() )
     }


     class VectorIsVectorLike extends VectorIsHilbertSpace with VectorLike[Vector[N], N]{

          def isZero(v: Vector[N]): Boolean = v.getElements().forall(e => e :==: Number[N].zero)

          def projection[R:RealNumber](v: Vector[N], onto: Vector[N])(implicit field: Field[N],
                                                                      r: Root[N,R]): Vector[N] =
               scale(onto, field.divide(innerProduct(v, onto), norm[R](onto)) )

          def outerProduct(v: Vector[N], w: Vector[N]): SetOfVectors[N] = {
               Util.Gen.ensureSize(v, w)

               val as: Seq[N] = v.getElements()
               val bs: Seq[N] = w.getElements()

               val result: Seq[Seq[N]] = as.map(a => bs.map(b => a * b))

               SetOfVectors.fromSeqs(result:_*)
          }

          def crossProduct(u: Vector[N], v: Vector[N])(implicit d: Dimension[Vector[N]]):
          Option[Vector[N]] = {

               if(u.dimension() == 3 && v.dimension() == 3){
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

     val monoid = new VectorIsMonoid
     val abelian = new VectorIsAbelianGroup
     val vectorSpace = new VectorIsVectorSpace
     val innerSpace = new VectorIsInnerProductSpace
     val normedSpace = new VectorIsNormedVectorSpace
     val hilbertSpace = new VectorIsHilbertSpace
     val vectorLike = new VectorIsVectorLike
     val dim = new VectorHasDimension
}


/*
trait VectorInstances {

     //TODO test whether not strictly necessary to have each one like this, can just have
     //the ending trait VectorLike as class and instance below like in ComplexIsNumber ...


     implicit def vectorIsMonoid[N: Number] = new VectorInstances[N].monoid
     implicit def vectorIsAbelianGroup[N: Number] = new VectorInstances[N].abelian
     //implicit def vectorIsVectorSpace[N: Number] = new VectorInstances[N].vectorSpace
     implicit def vectorIsInnerProductSpace[N: Number] = new VectorInstances[N].innerSpace
     implicit def vectorIsNormedVectorSpace[N: Number] = new VectorInstances[N].normedSpace
     implicit def vectorIsHilbertSpace[N: Number] = new VectorInstances[N].hilbertSpace
     implicit def vectorIsLikeAVector[N: Number] = new VectorInstances[N].vectorLike
}
*/


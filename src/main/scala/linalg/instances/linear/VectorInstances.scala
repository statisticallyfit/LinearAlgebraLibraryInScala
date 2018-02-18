package linalg.instances.linear

import linalg.implicits._
import linalg.kernel.{Number, RealNumber, Root, Trig}
import linalg.theory.{AbelianGroup, Field, Monoid}
import linalg.theory.space.{HilbertSpace, InnerProductSpace, NormedVectorSpace, VectorSpace}
import linalg.util.Util
import linalg.vector.{SetOfVectors, Vector, VectorLike}

import scala.collection.mutable.Seq

/**
  *
  */

class VectorThings[N: Number]{

     trait VectorIsMonoid extends Monoid[Vector[N]]{

          val zero: Vector[N] = Vector(Number.ZERO[N]) //just vector with one element

          def plus(v: Vector[N], w: Vector[N]): Vector[N] ={
               Util.Gen.ensureSize(v, w)
               Vector(v.getElements().zip(w.getElements()).map(pair => pair._1 + pair._2):_*)
          }
     }


     trait VectorIsAbelianGroup[N: Number] extends VectorIsMonoid[N] with AbelianGroup[Vector[N]]{

          def negate(v: Vector[N]): Vector[N] = Vector(v.getElements().map(e => e.negate()):_*)
     }


     trait VectorIsVectorSpace[N: Number] extends VectorIsAbelianGroup[N] with VectorSpace[Vector[N], N]{

          val one: Vector[N] = Vector(Number.ONE[N]) //just vector with one element

          def scale(v: Vector[N], factor: N): Vector[N] = Vector(v.getElements().map(e => e * factor):_*)

     }

     trait VectorIsInnerProductSpace[N: Number] extends VectorIsVectorSpace[N]
          with InnerProductSpace[Vector[N], N]{

          def innerProduct(v: Vector[N], w: Vector[N]): N = {
               Util.Gen.ensureSize(v, w)
               v.getElements().zip(w.getElements()).map(pair => pair._1 * pair._2).reduceLeft((acc, y) => acc + y)
          }
     }


     trait VectorIsNormedVectorSpace[N: Number] extends VectorIsInnerProductSpace[N]
          with NormedVectorSpace[Vector[N], N]{

          def norm[R:RealNumber](v: Vector[N])(implicit field: Field[N], root: Root[N, R]): N =
               Util.Gen.total[N](Vector(v.getElements().map(e => root.power(e, RealNumber[R].two)):_*))
     }


     trait VectorIsHilbertSpace[N: Number] extends VectorIsNormedVectorSpace[N]
          with HilbertSpace[Vector[N], N]{

          def angle[R:RealNumber](v: Vector[N], w: Vector[N])(implicit t: Trig[N],
                                                              field: Field[N], r: Root[N,R]): N =
               field.divide(innerProduct(v, w),  field.times(norm[R](v), norm[R](w)).arccos() )
     }

     //TODO need to extend VectorHasDimension from dimensioninstances

     trait VectorIsVectorLike[N: Number] extends VectorIsHilbertSpace[N] with VectorLike[Vector[N], N]{
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

          def crossProduct(u: Vector[N], v: Vector[N]): Option[Vector[N]] = {

               if(u.dimension() == 3 && v.dimension() == 3){
                    val w1: N = (u.get(2) * v.get(3)) - (u.get(3) * v.get(2))
                    val w2: N = (u.get(3) * v.get(1)) - (u.get(1) * v.get(3))
                    val w3: N = (u.get(1) * v.get(2)) - (u.get(2) * v.get(1))

                    Some(Vector(w1, w2, w3))

               } else None
          }
     }
}



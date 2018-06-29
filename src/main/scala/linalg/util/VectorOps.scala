package linalg.util

import linalg._
import linalg.implicits._
import linalg.vector.{SetOfVectors, Vector}
import spire.algebra.Eq

import scala.collection.mutable.Seq

/**
  *
  */


trait VectorOps {

     def plus[N: Number](v: Vector[N], w: Vector[N]): Vector[N] ={
          Util.ensureSize(v, w)
          Vector(v.getElements().zip(w.getElements()).map(pair => pair._1 + pair._2):_*)
     }

     def negate[N: Number](v: Vector[N]): Vector[N] = Vector(v.getElements().map(e => e.negate()):_*)

     def scale[N: Number](v: Vector[N], factor: N): Vector[N] = Vector(v.getElements().map(e => e * factor):_*)

     def innerProduct[N: Number](v: Vector[N], w: Vector[N]): N = {
          Util.ensureSize(v, w)
          v.getElements().zip(w.getElements()).map(pair => pair._1 * pair._2).reduceLeft((acc, y) => acc + y)
     }

     def norm[N: Number](v: Vector[N])(implicit field: Field[N], root: Root[N, N]): N = {

          val two: N = field.one + field.one
          val sum: N = Util.sumElements[N](Vector(v.getElements().map(e => root.power(e, two)):_*))
          sum.sqrt()
     }

     def angle[N: Number](v: Vector[N], w: Vector[N])(implicit t: Trig[N], field: Field[N], r: Root[N,N]): N =
          field.divide(innerProduct(v, w),  field.times(norm(v), norm(w)).arccos() )

     def isZero[N: Number](v: Vector[N]): Boolean = v.getElements().forall(e => e.isZero)

     def projection[N: Number](v: Vector[N], onto: Vector[N])(implicit field: Field[N], r: Root[N,N]): Vector[N] =
          scale(onto, field.divide(innerProduct(v, onto), norm(onto)) )

     def outerProduct[N: Number](v: Vector[N], w: Vector[N]): SetOfVectors[N] = {
          Util.ensureSize(v, w)

          val as: Seq[N] = v.getElements()
          val bs: Seq[N] = w.getElements()

          val result: Seq[Seq[N]] = as.map(a => bs.map(b => a * b))

          SetOfVectors.fromSeqs(result:_*)
     }

     def crossProduct[N: Number](u: Vector[N], v: Vector[N]): Option[Vector[N]] = {

          //TODO hacky way to get dimension/length but with Dimension doesn't work...
          if(u.size() == 3 && v.size() == 3){
               val w1: N = (u.get(2) * v.get(3)) - (u.get(3) * v.get(2))
               val w2: N = (u.get(3) * v.get(1)) - (u.get(1) * v.get(3))
               val w3: N = (u.get(1) * v.get(2)) - (u.get(2) * v.get(1))

               Some(Vector(w1, w2, w3))

          } else None
     }

     def dimension[N: Number](v: Vector[N]): Int = v.size()

     def eqv[N: Number](v: Vector[N], w: Vector[N]): Boolean ={
          v.getElements().zip(w.getElements())
               .forall(vwElemPair => vwElemPair._1 === vwElemPair._2)
     }

     def linearlyIndependent[N: Number](v: Vector[N], w: Vector[N]): Boolean =
          Util.isLinearlyIndependent(Util.colCombine(v, w))

     def isLinearlyIndependent[N: Number](v: Vector[N]): Boolean = true //yes a single vector is lin indep.





     // Utils

     def lengthCombine[N:Number](v: Vector[N], w: Vector[N]): Vector[N] =
          Vector((v.getElements() ++ w.getElements()):_*)

     def colCombine[N:Number](v: Vector[N], w: Vector[N]): SetOfVectors[N] =
          SetOfVectors(v, w)

     def sumElements[N:Number](v: Vector[N]): N = v.getElements().reduceLeft[N](_ + _)



     def ensureSize[N:Number](v: Vector[N], w: Vector[N], SIZE: Int = 0): Unit = {

          SIZE match {
               case 0 => if(v.size() != w.size()) {
                    throw Util.VectorLikeSizeException("Vectors are not same size; cannot continue operation.")
               }
               case _ => {
                    val len = v.size()

                    if (v.size() == w.size() && SIZE != len){
                         throw Util.VectorLikeSizeException("Vectors do not have same size as given size; cannot " +
                              "continue operation.")

                    } else if (v.size() != w.size()){
                         throw Util.VectorLikeSizeException("Vectors do not have same size; cannot continue operation.")
                    }
               }
          }
     }

}

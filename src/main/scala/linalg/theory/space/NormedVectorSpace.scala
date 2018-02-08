package linalg.theory.space

import cats.Eq

import linalg.numeric._
import linalg.theory._

/**
  *
  */

trait NormedVectorSpace[V, F] extends VectorSpace[V, F] {

     //this: Field[F] =>

     //note defining norm() just in normedinnerprodspace only - normedvecspace doesn't know about innerprod.
     def norm(v: V)(implicit f: Field[F]): F

     def normalize(v: V)(implicit f: Field[F]): V = scale(v, f.inverse(norm(v)))

     def isNormalized(v: V)(implicit eq: Eq[V], div: Field[F]): Boolean = eq.eqv(v, normalize(v))

     def distance(v: V, w: V)(implicit f: Field[F]): F = norm(plus(v, negate(w)))
}



object NormedVectorSpace extends NormedVectorSpaceBase  {
     final def apply[V, R](implicit ev: NormedVectorSpace[V, R]): NormedVectorSpace[V, R] = ev
}

private[space] trait NormedVectorSpaceBase {
     implicit def InnerProductSpaceIsNormedVectorSpace[V, F](implicit space: InnerProductSpace[V, F], nroot:
          Root[F]): NormedVectorSpace[V, F] = space.normed
}
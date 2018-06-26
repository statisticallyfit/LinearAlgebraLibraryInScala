package linalg.theory.space

import cats.Eq

import linalg._

/**
  *
  */

trait NormedVectorSpace[V, F] extends VectorSpace[V, F] {

     def norm(v: V)(implicit f: Field[F], r: Root[F, F]): F

     def normalize(v: V)(implicit field: Field[F], r: Root[F, F]): V = scale(v, field.inverse(norm(v)))

     def isNormalized(v: V)(implicit eq: Eq[V], f: Field[F], r: Root[F, F]): Boolean =
          eq.eqv(v, normalize(v))

     def distance(v: V, w: V)(implicit f: Field[F], r: Root[F, F]): F = norm(plus(v, negate(w)))
}



object NormedVectorSpace {
     @inline final def apply[V, F](implicit ev: NormedVectorSpace[V, F]): NormedVectorSpace[V, F] = ev
}

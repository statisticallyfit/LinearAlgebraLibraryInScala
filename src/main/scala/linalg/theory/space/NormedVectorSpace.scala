package linalg.theory.space

import cats.Eq

import linalg._

/**
  *
  */

trait NormedVectorSpace[V, F] extends VectorSpace[V, F] {

     //this: Field[F] =>

     //note defining norm() just in normedinnerprodspace only - normedvecspace doesn't know about innerprod.
     def norm(v: V)(implicit f: Field[F], r: Root[F, F]): F

     def normalize(v: V)(implicit field: Field[F], r: Root[F, F]): V = scale(v, field.inverse(norm(v)))

     def isNormalized(v: V)(implicit eq: Eq[V], f: Field[F], r: Root[F, F]): Boolean =
          eq.eqv(v, normalize(v))

     def distance(v: V, w: V)(implicit f: Field[F], r: Root[F, F]): F = norm(plus(v, negate(w)))
}



object NormedVectorSpace extends NormedVectorSpaceBase  {
     @inline final def apply[V, F](implicit ev: NormedVectorSpace[V, F]): NormedVectorSpace[V, F] = ev
}

private[space] trait NormedVectorSpaceBase {
     implicit def InnerProductSpaceIsNormedVectorSpace[V, F](implicit space: InnerProductSpace[V, F], nroot:
          Root[F,F]): NormedVectorSpace[V, F] = space.normed
}
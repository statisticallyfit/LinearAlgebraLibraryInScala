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
     def norm[R](v: V)(implicit f: Field[F], r: RootLike[F, R]): F

     def normalize[R](v: V)(implicit field: Field[F], r: RootLike[F, R]): V = scale(v, field.inverse(norm(v)))

     def isNormalized[R](v: V)(implicit eq: Eq[V], f: Field[F], r: RootLike[F, R]): Boolean = eq.eqv(v, normalize(v))

     def distance[R](v: V, w: V)(implicit f: Field[F], r: RootLike[F, R]): F = norm(plus(v, negate(w)))
}



object NormedVectorSpace extends NormedVectorSpaceBase  {
     final def apply[V, R](implicit ev: NormedVectorSpace[V, R]): NormedVectorSpace[V, R] = ev
}

private[space] trait NormedVectorSpaceBase {
     implicit def InnerProductSpaceIsNormedVectorSpace[V, F](implicit space: InnerProductSpace[V, F], nroot:
          Root[F]): NormedVectorSpace[V, F] = space.normed
}
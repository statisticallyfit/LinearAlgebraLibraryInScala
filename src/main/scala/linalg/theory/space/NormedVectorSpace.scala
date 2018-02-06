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
     def norm(v: V): F
     def normalize(v: V): V ={
          implicit val scalar: Field[F] = norm(v).asInstanceOf[Field[F]]
          scale(v, scalar.inverse(norm(v)))
     }
     def isNormalized(v: V)(implicit eq: Eq[V]): Boolean = eq.eqv(v, normalize(v))
     def distance(v: V, w: V): F = norm(plus(v, negate(w)))
}


object NormedVectorSpace extends NormedVectorSpaceBase  {
     final def apply[V, R](implicit ev: NormedVectorSpace[V, R]): NormedVectorSpace[V, R] = ev
}

private[space] trait NormedVectorSpaceBase {
     implicit def InnerProductSpaceIsNormedVectorSpace[V, F](implicit space: InnerProductSpace[V, F], nroot:
          Root[F, F]): NormedVectorSpace[V, F] = space.normed
}
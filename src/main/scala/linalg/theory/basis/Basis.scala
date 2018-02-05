package linalg.theory.basis

import linalg.theory.space._


/**
  *
  */
trait Basis[B, F] extends VectorSpace[B, F] /*extends Orthonormal[V] with Span[Basis[V, F], F]*/ {

     // this: Field[F] =>
     //this: VectorSpace[B, N] with Span[B, N] with LinearIndependence[B, N] =>

     //note ifvecset cols are linearly independent, then the vecset is a basis for vecpsace V^n,
     // if not return None.
     // which means this vecset is not a basis for the V^n vecspace. prereq is isBasisOfSpaceWith function
     def basis(): Option[B]
     //def isBasisOfSpaceWith(dim: Int): Boolean //todo do we really need this?
}

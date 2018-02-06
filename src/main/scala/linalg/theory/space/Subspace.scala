package linalg.theory.space

import linalg.theory._

/**
  * A non-empty subset S of vector space V is a subspace of V if it also
  * satisfies the ten axioms of a vector space.
  */
trait Subspace[S, F] extends VectorSpace[S, F] {

     //get a subspace of vecspace V (by getting a spanning set S) or by arbitrary declaration???
     //todo def subspace(implicit ev: Span[S, F]): S = ev.span
     def isSubsetOf(subset: S, parent: S): Boolean
     def isSubspaceOf(subspace: S, parent: S): Boolean
}


object Subspace {
     final def apply[S, R](implicit ev: Subspace[S, R]): Subspace[S, R] = ev
}

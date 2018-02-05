package linalg.theory.space



/**
  * A non-empty subset S of vector space V is a subspace of V if it also
  * satisfies the ten axioms of a vector space.
  */
trait Subspace[S, F] extends VectorSpace[S, F] {

     def isSubsetOf(subset: S, parent: S): Boolean
     def isSubspaceOf(subspace: S, parent: S): Boolean
}


object Subspace {
     final def apply[S, R](implicit ev: Subspace[S, R]): Subspace[S, R] = ev
}

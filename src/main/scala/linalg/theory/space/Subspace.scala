/*package linalg.theory.space


/**
  * A non-empty subset S of vector space V is a subspace of V if it also
  * satisfies the ten axioms of a vector space.
  */
trait Subspace[S, F] extends VectorSpace[S, F] {

     //return the basis of the subspace W of vecspace V, because that is the generating set of W (basis)
     def subspace(vset: S): S //same implementation as span(vset)

     def isSubset(entire: S, sub: S): Boolean

     //DEF 3.4 singh: nonempty subset S of vecspace V is called a subspace of V if
     // S is also a vector space with respect to the same vector addition and scalar
     // multiplication laws as V
     def isSubspace(entire: S, sub: S): Boolean
}


object Subspace {
     @inline final def apply[S, R](implicit ev: Subspace[S, R]): Subspace[S, R] = ev
}*/

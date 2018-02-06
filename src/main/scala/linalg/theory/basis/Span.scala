package linalg.theory

import linalg.numeric._
import linalg.theory.space._
import linalg.vector._

/**
  *
  */

trait Span[V, F] extends VectorSpace[V, F] {

     def span(vset: V): V //gets a set that spans/generates the generic vecspace (gets the basis) todo

     //does vecset span/generate the generic space V (which is like R3)? (example 3.10) - is genspace V spanned by vset?
     //LinearAlgebraToolkit: "Determining of the set spans the spance"
     //def isSpanned(vset: VectorSet[F]): Boolean //= vset.reducedRowEchelonForm() === VectorSet.IDENTITY(vset)
     def isSpanned(vset: V): Boolean

     //does vecset span/generate a specific vector? (example 3.11)
     //def isSpanned(vset: VectorSet[F], space: V): Boolean
     //todo implicit conversion from Set[V] to VectorSet[V]
     def isSpanned(vset: Set[V], v: V): Boolean

     //gets the coefficients the relate the vset to the vector v in the linear combination. They are:
     // k1v1 + k2v2 + k3v3 + ... = v, where vset = {v1,v2,v3...} and v = v.
     def getSpanningCoefficients(vset: Set[V], v: V): Option[Set[F]] //if isspanned is false, then NOne else Some(...)
}


package linalg.theory

import linalg.numeric._
import linalg.theory.space._
import linalg.vector._

/**
  *
  */
//todo theorem 4.2.6 in howard: implement Eq for Span type


trait Span[V, F] extends VectorSpace[V, F] {

     //gets a set that spans/generates the generic vecspace (gets the basis) todo
     def span(vset: SetOfVectors[F]): SetOfVectors[F] // = vset.reducedRowEchelonForm()

     //does vecset span/generate the generic space V (which is like R3)? (example 3.10 singh) - is genspace V spanned
     // by vset?
     //page 198 howard
     //LinearAlgebraToolkit: "Determining if the set spans the space"
     //def isSpanned(vset: VectorSet[F]): Boolean //= vset.reducedRowEchelonForm() === VectorSet.IDENTITY(vset)
     def isSpanned(vset: SetOfVectors[F]): Boolean

     //does vecset span a specific vector? (example 3.11 singh) Is single vec v spanned by vset? is it in span of
     // vset
     //page 197 howard
     //def isSpanned(vset: VectorSet[F], space: V): Boolean
     //todo implicit conversion from Set[V] to VectorSet[V]
     def isSpanned(vset: SetOfVectors[F], v: V): Boolean
     def isInSpanOf(v: V, vset: SetOfVectors[F]): Boolean = isSpanned(vset, v)

     //gets the coefficients the relate the vset to the vector v in the linear combination. They are:
     // k1v1 + k2v2 + k3v3 + ... = v, where vset = {v1,v2,v3...} and v = v.
     def getSpanningCoefficients(vset: SetOfVectors[F], v: V): Option[SetOfVectors[F]] //if isspanned is false, then NOne
     // else Some(...)
}


object Span {
     final def apply[S, R](implicit ev: Span[S, R]): Span[S, R] = ev
}
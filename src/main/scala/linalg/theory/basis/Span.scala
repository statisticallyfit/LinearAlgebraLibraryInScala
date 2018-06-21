package linalg.theory.basis

import linalg._

import cats.Eq

/**
  *
  */
//todo theorem 4.2.6 in howard: implement Eq for Span type


trait Span[V, F] {

     // Returns a set that spans/generates the generic vecspace (gets the basis)
     def span(vset: V)(implicit s: SetVecLike[V, F]): V  = s.rowReducedEchelon(vset)

     // If the system is consistent then it spans the space R#, where # = numrows.
     // Example 3.10 - singh book
     // page 198 = howard book
     def doesSetSpanTheSpace(vset: V)(implicit s: LinearSystem[V, F]): Boolean =
          s.isConsistent(vset)
     // see snagit image in kuldeep folder - if the numrows-by-numrows identity matrix
     // on the right as augmented matrix with vecset has rref that is inconsistent (zero row
     // equaling constants) then return false, else true

     //does vecset span a specific vector? (example 3.11 singh) Is single vec v spanned by vset? is it in span of
     // vset
     //page 197 howard
     //def isSpanned(vset: VectorSet[F], space: V): Boolean
     //todo implicit conversion from Set[V] to VectorSet[V]

     //todo choice - let V implement VectorLike OR let V <: Vector[F] -- which one? in case Poly exteds Vector
     // implements vlike doesn't work
     //def isSpanned[V](vset: W, v: V)(implicit vecLike: VectorLike[V, F]): Boolean
     def doesSetSpanTheVector(vset: V, v: V): Boolean
     //def isInSpanOf[V](v: V, vset: W)(implicit vecLike: VectorLike[V, F]): Boolean = isSpanned(vset, v)
     def isInSpanOf(v: V, vset: V): Boolean = doesSetSpanTheVector(vset, v)

     //gets the coefficients the relate the vset to the vector v in the linear combination. They are:
     // k1v1 + k2v2 + k3v3 + ... = v, where vset = {v1,v2,v3...} and v = v.
     //def getSpanningCoefficients[V](vset: W, v: V)(implicit vecLike: VectorLike[V, F]): Option[W] //if isspanned is
     // false, then NOne
     def getSpanningCoefficients(vset: V, v: V): Option[V]
     // else Some(...)
}


object Span {
     @inline final def apply[V, F](implicit ev: Span[V, F]): Span[V, F] = ev
}
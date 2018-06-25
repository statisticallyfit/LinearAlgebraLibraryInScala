package linalg.theory.basis

import linalg._

import cats.Eq

/**
  *
  */
//todo theorem 4.2.6 in howard: implement Eq for Span type

//abstract class Span[V[_], W[_], F](implicit vec: VectorLike[V[F], F], w: SetVecLike[W[F], F]){

trait Span[V, W, F]{

     // Returns a set that spans/generates the generic vecspace (gets the basis)
     def span(vset: W): W  //= s.rowReducedEchelon(vset)

     // If the system is consistent then it spans the space R#, where # = numrows.
     // Example 3.10 - singh book
     // page 198 = howard book
     //TODO
     def doesSetSpanTheSpace(vset: W): Boolean
     //step 1 - set up augmented matrix which implements linear system.
     //step 2 - check is consistent.
     // see snagit image in kuldeep folder - if the numrows-by-numrows identity matrix
     // on the right as augmented matrix with vecset has rref that is inconsistent (zero row
     // equaling constants) then return false, else true

     //does vecset span a specific vector? (example 3.11 singh) Is single vec v spanned by vset?
     // is it in span of vset?
     //page 197 howard
     def doesSetSpanTheVector(vset: W, v: V): Boolean
     //step 1 - set up augmented matrix with W | V and solve - check if consistent

     def isSpannedBy(v: V, vset: W): Boolean = doesSetSpanTheVector(vset, v)

     // Gets the coefficients the relate the vset to the vector v in the linear combination.
     // They are:  k1v1 + k2v2 + k3v3 + ... = v, where vset = {v1,v2,v3...} and v = v.
     def getSpanningCoefficients(vset: W, v: V): Option[Seq[F]]
}


object Span {
     @inline final def apply[V, W, F](implicit ev: Span[V, W, F]): Span[V, W, F] = ev
}
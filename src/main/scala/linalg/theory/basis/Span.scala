package linalg.theory.basis


/**
  *
  */
trait Span[V, W, F]{

     // Returns a set that spans/generates the generic vecspace (gets the basis)
     def span(vset: W): W //see page 198 poole, another way to do it without
     // finding the basis, checking which set of vectors is linear independent.

     // If the system is consistent then it spans the space R#, where # = numrows.
     // Example 3.10 - singh book, page 198 = howard book
     def isSpanningSpace(vset: W, dim: Int): Boolean

     //does vecset span a specific vector? (example 3.11 singh) Is single vec v spanned by vset?
     // is it in span of vset? /page 197 howard
     def isSpanningVector(vset: W, v: V): Boolean

     def isInSpan(v: V, vset: W): Boolean = isSpanningVector(vset, v)

     // Gets the coefficients the relate the vset to the vector v in the linear combination.
     // They are:  k1v1 + k2v2 + k3v3 + ... = v, where vset = {v1,v2,v3...} and v = v.
     def getSpanningCoefficients(vset: W, v: V): Option[W]
}


object Span {
     @inline final def apply[V, W, F](implicit ev: Span[V, W, F]): Span[V, W, F] = ev
}
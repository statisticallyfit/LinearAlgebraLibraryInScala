package linalg.theory.space

import scala.language.higherKinds
import scala.language.implicitConversions

/**
  * An inner product on a real vector space V is an operation <,> which assigns
  * a unique real number to each pair of vectors, u, and v, which satisfies the
  * following axioms for all vectors u,v,w in V and all scalars k.
  *
  * Laws:
  * (i) < u, v> = < v, u >                   --- commutative law
  * (ii) < u+v, w > = < u,w > + < v,w >      --- distributive law
  * (iii) < ku, v > = k< u,v >               --- taking out scalar k
  * (iv) < u,u > >= 0 and we have < u,u> = 0 if and only if u = 0
  *                                          --- (means the inner product is zero or positive)
  *
  */
trait InnerProductSpace[I, F] extends VectorSpace[I, F] {

     def innerProduct(i1: I, i2: I): F
     def dotProduct(i1: I, i2: I): F = innerProduct(i1, i2)
}

object InnerProductSpace {
     @inline final def apply[I, R](implicit ev: InnerProductSpace[I, R]): InnerProductSpace[I, R] = ev
}

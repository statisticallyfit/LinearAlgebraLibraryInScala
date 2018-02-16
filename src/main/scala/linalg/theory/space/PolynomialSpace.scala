package linalg.theory.space

/**
  * todo
  * note: polynomials are represented with highest powers in first place of array.
  */
trait PolynomialSpace[P, F] extends linalg.ContinuousFunctionSpace[P, F] {

}

object PolynomialSpace {
     @inline final def apply[P, F](implicit ev: PolynomialSpace[P, F]): PolynomialSpace[P, F] = ev
}

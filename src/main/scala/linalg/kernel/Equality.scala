package linalg.kernel

import cats.Eq

/**
  *
  */
//note: might as well declare Eq because Functor in cats uses it, so don't need TWO equals methods.
trait Equality[E] extends Eq[E] {
     //def equal(x: E, y: E): Boolean
     def lessThan(x: E, y: E): Boolean
     def greaterThan(x: E, y: E): Boolean = lessThan(y, x)
     def lessThanOrEqual(x: E, y: E): Boolean = lessThan(x, y) || eqv(x, y)
     def greaterThanOrEqual(x: E, y: E): Boolean = greaterThan(x, y) || eqv(x, y)
}

object Equality {
     @inline final def apply[E](implicit ev: Equality[E]): Equality[E] = ev
}
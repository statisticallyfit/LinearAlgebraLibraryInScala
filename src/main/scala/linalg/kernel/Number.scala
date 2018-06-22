package linalg.kernel


import linalg.theory.Field


import scala.language.implicitConversions
import scala.language.higherKinds

/**
  *
  */

trait Number[N] extends Field[N] with Absolute[N, N] with Equality[N] {

     val two: N = plus(one, one)

     def minus(x: N, y: N): N = plus(x, negate(y))

     def isZero(x: N): Boolean //= eqv(zero, x)
     def isNegative(x: N): Boolean

     def doubleValue(x: N): Double
     def from(x: Int): N
     //def to(x: Number[N]): N
}

object Number {

     def ZERO[N](implicit gen: Number[N]): N = gen.zero
     def ONE[N](implicit gen: Number[N]): N = gen.one
     def TWO[N](implicit gen: Number[N]): N = gen.two

     @inline final def apply[N](implicit ev: Number[N]): Number[N] = ev
}




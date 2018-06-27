package linalg.kernel


import linalg.theory.Field


import scala.language.implicitConversions
import scala.language.higherKinds

/**
  *
  */

//note: cannot inherit Absolute here since Complex won't work as desired. Can do this with
// Equality though because just one parameter N. So now Number - Is - Equality but need
// to give separate absolute implicit in methods using absolute... whereas can use equality
//things directly when calling numbe     r.
trait Number[N] extends Field[N] /*with Absolute[N, N] with Root[N,N]*/ with Equality[N] {

     val two: N = plus(one, one)

     def minus(x: N, y: N): N = plus(x, negate(y))

     def isZero(x: N): Boolean //= eqv(zero, x)
     def isNegative(x: N): Boolean

     def conjugate(x: N): N

     def doubleValue(x: N): Double
     def denominator(x: N): Int
     def from(x: Int): N
     //def to(x: Number[N]): N
}

object Number {

     def ZERO[N](implicit gen: Number[N]): N = gen.zero
     def ONE[N](implicit gen: Number[N]): N = gen.one
     def TWO[N](implicit gen: Number[N]): N = gen.two

     @inline final def apply[N](implicit ev: Number[N]): Number[N] = ev
}




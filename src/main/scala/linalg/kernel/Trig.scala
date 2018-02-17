package linalg.kernel

import linalg.theory.Field

/**
  *
  */
trait Trig[T] extends Field[T] {

     val E: T
     val PI: T

     def sin(x: T): T
     def cos(x: T): T
     def tan(x: T): T
     def csc(x: T): T = divide(one, sin(x))
     def sec(x: T): T = divide(one, cos(x))
     def cot(x: T): T = divide(one, tan(x))

     def arcsin(x: T): T
     def arccos(x: T): T
     def arctan(x: T): T
     def arccsc(x: T): T = divide(one, arcsin(x))
     def arcsec(x: T): T = divide(one, arccos(x))
     def arccot(x: T): T = divide(one, arctan(x))

     //returns the theta component of polar (r, theta) of the x-y coordinate (x: T, y: T)
     def theta(y: T, x: T): T = tan(divide(y, x))

     //TODO do the sinh + hyperbolic functions + log + exp
}

object Trig {
     @inline final def apply[T](implicit ev: Trig[T]): Trig[T] = ev
}

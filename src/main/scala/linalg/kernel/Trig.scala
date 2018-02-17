package linalg.kernel

import linalg.theory.Field

/**
  *
  */
trait Trig[T] /*extends Field[T]*/ {

     val E: T
     val PI: T

     def sin(x: T): T
     def cos(x: T): T
     def tan(x: T): T
     def csc(x: T)(implicit f: Field[T]): T = f.divide(f.one, sin(x))
     def sec(x: T)(implicit f: Field[T]): T = f.divide(f.one, cos(x))
     def cot(x: T)(implicit f: Field[T]): T = f.divide(f.one, tan(x))

     def arcsin(x: T): T
     def arccos(x: T): T
     def arctan(x: T): T
     def arccsc(x: T)(implicit f: Field[T]): T = f.divide(f.one, arcsin(x))
     def arcsec(x: T)(implicit f: Field[T]): T = f.divide(f.one, arccos(x))
     def arccot(x: T)(implicit f: Field[T]): T = f.divide(f.one, arctan(x))

     //returns the theta component of polar (r, theta) of the x-y coordinate (x: T, y: T)
     def theta(y: T, x: T)(implicit f: Field[T]): T = tan(f.divide(y, x))

     //TODO do the sinh + hyperbolic functions + log + exp
}

object Trig {
     @inline final def apply[T](implicit ev: Trig[T]): Trig[T] = ev
}

package linalg.kernel

/**
  *
  */
trait Trig[T] {

     val E: T
     val PI: T

     def sin(x: T): T
     def cos(x: T): T
     def tan(x: T): T
     def csc(x: T): T
     def sec(x: T): T
     def cot(x: T): T

     def arcsin(x: T): T
     def arccos(x: T): T
     def arctan(x: T): T
     def arccsc(x: T): T
     def arcsec(x: T): T
     def arccot(x: T): T

     //returns the theta component of polar (r, theta) of the x-y coordinate (x: T, y: T)
     def theta(y: T, x: T): T
}

object Trig {
     @inline final def apply[T](implicit ev: Trig[T]): Trig[T] = ev
}

package linalg.kernel

/**
  *
  */

trait RealNumber[R] extends Number[R] with Root[R,R] with Absolute[R,R]


object RealNumber {
     def ZERO[R](implicit gen: RealNumber[R]): R = gen.zero
     def ONE[R](implicit gen: RealNumber[R]): R = gen.one
     def TWO[R](implicit gen: RealNumber[R]): R = gen.two

     @inline final def apply[R](implicit ev: RealNumber[R]): RealNumber[R] = ev
}
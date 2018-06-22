package linalg.kernel

/**
  *
  */

trait Absolute[A, B] {
     def absoluteValue(x: A): B
}

object Absolute {
     @inline final def apply[A,B](implicit ev: Absolute[A,B]): Absolute[A,B] = ev
}

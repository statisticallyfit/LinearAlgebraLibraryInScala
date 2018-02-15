package linalg.kernel


trait Absolute[N, R] {
     def absoluteValue(x: N): R
}

object Absolute {
     @inline final def apply[N, R](implicit ev: Absolute[N, R]): Absolute[N, R] = ev
}

trait Abs[A] extends Absolute[A, A]

object Abs {
     @inline final def apply[A](implicit ev: Abs[A]): Abs[A] = ev
}
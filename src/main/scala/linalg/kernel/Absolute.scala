package linalg.kernel

/**
  *
  */

/*trait AbsoluteLike[N, R] {
     def absoluteValue(x: N): R
}

object AbsoluteLike {
     @inline final def apply[N, R](implicit ev: AbsoluteLike[N, R]): AbsoluteLike[N, R] = ev
}*/

trait Absolute[A, B] {
     def absoluteValue(x: A): B
}

object Absolute {
     @inline final def apply[A,B](implicit ev: Absolute[A,B]): Absolute[A,B] = ev
}

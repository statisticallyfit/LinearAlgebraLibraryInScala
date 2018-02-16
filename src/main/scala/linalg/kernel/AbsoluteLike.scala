package linalg.kernel


trait AbsoluteLike[N, R] {
     def absoluteValue(x: N): R
}

object AbsoluteLike {
     @inline final def apply[N, R](implicit ev: AbsoluteLike[N, R]): AbsoluteLike[N, R] = ev
}

trait Absolute[A] extends AbsoluteLike[A, A]
/*{
     def absoluteValue(x: A): A
}*/
object Absolute {
     @inline final def apply[A](implicit ev: Absolute[A]): Absolute[A] = ev
}

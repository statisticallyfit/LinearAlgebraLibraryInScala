

trait AbsLike[N, R]{
     def absolute(x: N): R
}
trait RootLike[N, R]{
     def power(base: N, exp: R): N
}
trait TrigLike[T] {
     def sin(t: T): T
}
trait EqLike[T] {
     def lessThan(x: T, y: T): Boolean
}
trait NumberLike[N] extends AbsLike[NumberLike[N], N]
     with RootLike[NumberLike[N], N] with TrigLike[N] with EqLike[N] {

     def plus(x: N, y: N): N
}

trait Number[N] extends NumberLike[N]{
     //def from(x: Int): N
}
trait RealNumber[R] extends Number[R]

case class Complex[R: RealNumber](re: R, im: R)

object NumberLike {
     import implicitSyntax._

     implicit def ComplexIsNumber[R:RealNumber] = new Number[Complex[R]]{
          def plus(x: Complex[R], y:Complex[R]): Complex[R] = Complex(x.re + y.re, x.im + y.im)
          def power(x: Complex[R], exp: R): Complex[R] = x //just something
          def absolute(x: Complex[R]): R = x.re
          def sin(x: Complex[R]): Complex[R] = x
          def lessThan(x: Complex[R], y: Complex[R]): Boolean = true
          //def from(x: Int): Complex[R]
     }

     implicit object IntIsRealNumber extends RealNumber[Int]{
     }

}

object implicitSyntax {

     implicit class NumberLikeOps[N](current: N)(implicit nlike: NumberLike[N]){
          def +(that: N): N = nlike.plus(current, that)
     }
     Complex()
}
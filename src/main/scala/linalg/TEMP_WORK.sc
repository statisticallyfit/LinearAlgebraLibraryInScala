
import scala.language.higherKinds
import scala.language.implicitConversions


trait RootLike[N, R]{
     def power(base: N, exp: R): N
}
trait Root[R] extends RootLike[R, R]

trait NumberLike[N[_], R] {

     implicit def root: RootLike[Number[N[R]], RealNumber[R]] //todo maybe numlike not number[n]??
}

trait Number[N] extends NumberLike[Number[N], RealNumber[N]]{
     def plus(x: N, y: N): N
}
trait RealNumber[R] extends Number[R]{
     implicit def realRoot: Root[R]
     override def root: RootLike[R, R] = realRoot
}
case class Complex[R: RealNumber](re: R, im: R)

object implicitSyntax {
     implicit class NumberOps[N](current: N)(implicit nlike: Number[N]){
          def +(that: N): N = nlike.plus(current, that)
     }
     implicit class NumLikeOps[N[_], R: RealNumber](current: Number[N[R]])
                                                   (implicit n: NumberLike[N[R],R],
                                                    n1: Number[N[R]]){

          val r: RootLike[N, R] = n.root

          def nRoot(exp: R): N[R] = r.power(current, exp)
     }

     import NumberLike._
     Complex(1,2).nRoot(2)
     Complex(1,2) + Complex(2,3)
}


object NumberLike {
     import implicitSyntax._

     implicit def ComplexIsNumber[R:RealNumber] = new Number[Complex[R]]{
          def plus(x: Complex[R], y:Complex[R]): Complex[R] = Complex(x.re + y.re, x.im + y.im)

          implicit def root: RootLike[Complex[R], R] = new RootLike[Complex[R], R]{
               def power(x: Complex[R], exp: R): Complex[R] = x
          }
     }

     implicit object IntIsRealNumber extends RealNumber[Int]{
          def plus(x: Int, y: Int): Int = x + y

          implicit def realRoot: Root[Int] = new Root[Int] {
               def power(base: Int, exp: Int): Int = base ^ exp
          }
     }

}

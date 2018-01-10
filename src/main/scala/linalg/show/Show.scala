package linalg.show

import linalg.numeric._


trait Show[S] {

     def show(x: S): String
}
object Show {
     implicit class ShowOps[S: Show](current: S) {
          val ev = implicitly[Show[S]]

          def show: String = ev.show(current)
     }

     implicit object IntHasShow extends Show[Int] {def show(x: Int): String = x.toString}
     implicit object DoubleHasShow extends Show[Double] {def show(x: Double): String = x.toString}
     implicit object RealHasShow extends Show[Real] { def show(x: Real): String = x.double.toString }

     implicit object RationalHasShow extends Show[Rational] {
          def show(x: Rational): String = x.den match {
               case 1 => x.num.toString
               case _ => x.num.toString + "/" + x.den.toString
          }
     }

     implicit def ComplexHasShow[R : RealNumber] = new Show[Complex[R]] {
          def show(x: Complex[R]): String = x.re.toString + Imaginary(x.im).toString
     }
}
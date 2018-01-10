package linalg.numeric

/**
  *
  */
object complextest {

     sealed trait ComplexNumber[N] {
          val re: N
          val im: N
     }

     case class Compp[N](re: N, im: N) extends ComplexNumber[N]

     case class RealN(re: Int) extends ComplexNumber[Int] { val im: Int = 0 }
     implicit class ToReal(val re: Int) extends AnyVal {
          def +(that: ImaginaryN) = Mixed(re, that.im)
     }

     case class ImaginaryN(im: Int) extends ComplexNumber[Int] { val re: Int = 0 }
     implicit class ToImaginary(val im: Int) extends AnyVal {
          def i: ImaginaryN = ImaginaryN(im)
     }

     case class Mixed(re: Int, im: Int) extends ComplexNumber[Int] {
          override def toString: String = re + " + " + im + "i"
     }

     //------
     case class Compl(re: RE, im: RE)
     case class RE(d: Double) {
          def i: Compl = new Compl(RE(0), this)
     }


     def main(args: Array[String]) {


     }

}


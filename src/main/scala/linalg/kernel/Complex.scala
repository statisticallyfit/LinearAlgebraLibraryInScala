package linalg.kernel


import linalg.implicits._

/**
  *
  */

case class Complex[R:RealNumber](re:R, im:R) {
    override def toString: String = Show[Complex[R]].show(this)
}

object Complex {

     def ZERO[R](implicit gen: RealNumber[R]): Complex[R] = new Complex(gen.zero, gen.zero)
     def ONE[R](implicit gen: RealNumber[R]): Complex[R] = new Complex(gen.one, gen.zero)
     def TWO[R](implicit gen: RealNumber[R]): Complex[R] = new Complex(gen.two, gen.zero)

     def apply[R](realPart: R)(implicit gen: RealNumber[R]): Complex[R] = new Complex(realPart, gen.zero)


     // --- Operations ---
     /*(implicit rr: Root[R], t: Trigonometric[R])*/
     def isReal[R: RealNumber](x: Complex[R]): Boolean = x.im.isZero

     def isImaginary[R: RealNumber](x: Complex[R]): Boolean = !isReal(x)

     def polar[R: RealNumber](z: Complex[R]): Complex[R] = Complex(magnitude(z), angle(z))

     def magnitude[R: RealNumber](z: Complex[R])/*(implicit rr: Root[R])*/: R =
          (z.re * z.re + z.im * z.im).sqrt()

     //just returns the value of theta for the complex number: theta = arctan(b / a), where c = a + bi
     /*(implicit trig: Trigonometric[R])*/
     def angle[R](z: Complex[R])(implicit realNum: RealNumber[R]): R = z.re.theta(z.im)

     /** Returns the nth root of a complex number - in tuple form = (modulus root n, list of all roots) */
     def nthRootComplex[R](z: Complex[R], n: R)(implicit gen: RealNumber[R],
                                                trig: Trig[R])/*,
                                                rr: Root[R])*/: (R, List[R]) ={

          val two: R = gen.one + gen.one
          val polarComplex: Complex[R] = polar(z)
          val (modulus, theta): (R, R) = (polarComplex.re, polarComplex.im)

          val theNRoots: List[R] = List.tabulate[R](n.toInt)(k => (theta + two * gen.from(k) * trig.PI) / n)

          (modulus.nRoot(n), theNRoots)
     }

     def nthRootsOfUnity[R](z: Complex[R], n: R)(implicit gen: RealNumber[R], trig: Trig[R]): List[R] = {
          val two: R = gen.one + gen.one
          List.tabulate[R](n.toInt)(k => (two * gen.from(k) * trig.PI) / n)
     }

     def conjugate[R: RealNumber](z: Complex[R]): Complex[R] = Complex(z.re, z.im.negate())
}

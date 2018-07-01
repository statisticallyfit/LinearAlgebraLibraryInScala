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
     def apply[R: RealNumber](realPart: R, imagPart: R): Complex[R] = new Complex(realPart, imagPart)

     // --- Operations ---
     /*(implicit rr: Root[R], t: Trigonometric[R])*/
     def isReal[R: RealNumber](x: Complex[R]): Boolean = x.im.isZero

     def isImaginary[R: RealNumber](x: Complex[R]): Boolean = !isReal(x)

     def polar[R: RealNumber](z: Complex[R]): Complex[R] = Complex(magnitude(z), angle(z))

     def magnitude[R: RealNumber](z: Complex[R]): R = (z.re * z.re + z.im * z.im).sqrt()

     //just returns the value of theta for the complex number: theta = arctan(b / a), where c = a + bi
     /*(implicit trig: Trigonometric[R])*/
     def angle[R](z: Complex[R])(implicit realNum: RealNumber[R]): R = z.re.theta(z.im)

     // z^n = |z|^n * (cos(n*theta) + i sin(n*theta))
     //rule: n must be whole number
     def powerDeMoivre[R](z: Complex[R], p: Int)(implicit gen: RealNumber[R]): Complex[R] ={
          val theta: R = angle(z)
          val n: R = gen.from(p)

          magnitude(z) * Complex( (n*theta).cos(), (n*theta).sin() )
     }

     /** Returns the nth root of a complex number - in tuple form = (modulus root n, list of all roots) */
     //note: n must be integer form, CANNOT be double or rational RealNumber type
     def nthRootComplex[R](z: Complex[R], n: Int)(implicit gen: RealNumber[R],
                                                trig: Trig[R]): (R, List[R]) ={

          val polarComplex: Complex[R] = polar(z)
          val (modulus, theta): (R, R) = (polarComplex.re, polarComplex.im)

          val theNRoots: List[R] = List.tabulate[R](n)(k => //goes from 0,1,2 ... (k-1)
               (theta + gen.two * gen.from(k) * trig.PI) / gen.from(n))

          (modulus.nRoot(gen.from(n)), theNRoots)
     }

     def nthRootsOfUnity[R](z: Complex[R], n: Int)(implicit gen: RealNumber[R], trig: Trig[R]): List[R] = {
          List.tabulate[R](n)(k => (gen.two * gen.from(k) * trig.PI) / gen.from(n))
     }

     def conjugate[R: RealNumber](z: Complex[R]): Complex[R] = Complex(z.re, z.im.negate())
}

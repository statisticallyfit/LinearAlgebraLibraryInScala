package linalg.kernel


import linalg.show.Show._
import linalg.theory._
import linalg.implicits._
import linalg.kernel.NumericConversion._
/*import linalg.syntax.AbsoluteSyntax._
import linalg.syntax.CompareSyntax._
import linalg.syntax.RootSyntax._
import linalg.syntax.TrigSyntax._*/

import cats.Eq
import cats.Monoid

import org.apache.commons.lang3.math.Fraction

import scala.language.implicitConversions
import scala.language.higherKinds


/**
  * A basic Number system that supports trigonometry, equality and different number types: Complex, Real, Rational,
  * Int, Double.
  * Functionality is implemented using a typeclass pattern to preserve cleanliness and maintainability.
  *
  *
  * Features:
  * - rational number reducability upon creation
  * - complex number .i creation
  * - complex numbers can have rational arguments too, anything that implements the RealNumber typeclass.
  * - printing occurs via neat Show trait, just like in Haskell.
  * - interoperability between Complex[R] and R types.
  * - trigonometric Real and Double types
  * - complex roots of unity and nth roots functions.
  *  - Trig implementations - making RealLike extend Trig so when we declare Reals we must declare Trigs too.
  *
  *
  * note: Source for complex .i accessor:
  * https://stackoverflow.com/questions/17381896/scala-simple-notation-of-imaginary-number
  */
//TODO: Number: make self-type this: Absolute[Number[N], N] ... etc - if it works?
// TODO so that RealLike inherits - but must  also include: this: Trig[R] =>

// TODO   must change Conversion trait so that we have Number[N, R] and RealLike[R] extends
// TODO Number[R,R] and we have Number[Complex[R],R] instance and the rest are RealLike-s.
// TODO Then in the Number trait we add conversion methods: def plus(const: R, num: N): N
// TODO and def plus(num: N, const: R): N and good old def plus(n1: N, n2: N): N  and
// TODO so on for each operation. Then maybe this will guarantee type conversion? Like
// TODO Real + complex => complex ... so no need for the hardcoded-complex-int conversion,
// TODO we have interoperability automatically.




//note cannot have extending AbsoluteLike because of same old problem, so just implement them separately in the
//implicit typeclass declaration

trait Number[N] extends Field[N] with Trig[N] with Equality[N] with Eq[N] {

     val two: N = plus(one, one)

     def minus(x: N, y: N): N = plus(x, negate(y))

     def isZero(x: N): Boolean = eqv(zero, x)
     def isNegative(x: N): Boolean

     def doubleValue(x: N): Double
     def from(x: Int): N
}

object Number {

     def ZERO[N](implicit gen: Number[N]): N = gen.zero
     def ONE[N](implicit gen: Number[N]): N = gen.one
     def TWO[N](implicit gen: Number[N]): N = gen.two

     @inline final def apply[N](implicit ev: Number[N]): Number[N] = ev
}



trait RealNumber[R] extends Number[R] with Abs[R] with Root[R]


object RealNumber {
     def ZERO[R](implicit gen: RealNumber[R]): R = gen.zero
     def ONE[R](implicit gen: RealNumber[R]): R = gen.one
     def TWO[R](implicit gen: RealNumber[R]): R = gen.two

     @inline final def apply[R](implicit ev: RealNumber[R]): RealNumber[R] = ev
}

// ---------------------------------------------------------------------------------------------------------


case class Real(double: Double) {
     override def toString = Real(double).show
}


case class Rational(private val n: Int, private val d: Int) {
     val reduced: Fraction = Fraction.getFraction(n, d).reduce()
     val num: Int = reduced.getNumerator
     val den: Int = reduced.getDenominator

     override def toString: String = Rational(num, den).show
}


case class Complex[R:RealNumber](re:R, im:R) {
     override def toString: String = Complex(re, im).show
}


case class Imaginary[R: RealNumber](im: R) {

     implicit def i: Imaginary[R] = this

     override def toString: String = im match {
          case _: Rational => im.isNegative match {
               case true => " - (" + im.negate().toString + ")" + "i"
               case false => " + (" + im.toString + ")" + "i"
          }
          case _ => im.isNegative match {
               case true => " - " + im.negate().toString + "i"
               case false => " + " + im.toString + "i"
          }
     }
}





object Complex {

     def ZERO[R](implicit gen: RealNumber[R]): Complex[R] = new Complex(gen.zero, gen.zero)
     def ONE[R](implicit gen: RealNumber[R]): Complex[R] = new Complex(gen.one, gen.zero)
     def TWO[R](implicit gen: RealNumber[R]): Complex[R] = new Complex(gen.two, gen.zero)

     def apply[R](realPart: R)(implicit gen: RealNumber[R]): Complex[R] = new Complex(realPart, gen.zero)


     // --- Operations ---
     /*(implicit rr: Root[R], t: Trigonometric[R])*/
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


object Imaginary {

     def ZERO[R](implicit gen: RealNumber[R]): Imaginary[R] = new Imaginary(gen.zero)
     def ONE[R](implicit gen: RealNumber[R]): Imaginary[R] = new Imaginary(gen.one)
     def TWO[R](implicit gen: RealNumber[R]): Imaginary[R] = new Imaginary(gen.two)
}


object Real {
     val ZERO: Real = new Real(0)
     val ONE: Real = new Real(1)
     val TWO: Real = new Real(2)
}

object Rational {
     val ZERO: Rational = new Rational(0, 1)
     val ONE: Rational = new Rational(1, 1)
     val TWO: Rational = new Rational(2, 1)

     def apply(numerator: Int): Rational = new Rational(numerator, 1)

     def apply(fractionAsDouble: Double): Rational = {
          val f = Fraction.getFraction(fractionAsDouble).reduce()
          new Rational(f.getNumerator, f.getDenominator)
     }
}


// ---------------------------------------------------------------------------------------------------------



object NumberTester extends App {


     import Number._

     val a: Complex[Rational] = Rational(3,5) + Rational(2, 4).i + Rational(1)
     val b: Complex[Int] = 3 + 5.i + 3
     val c: Complex[Int] = 1 - 2.i

     val r1: Rational = Rational(2)
     val r2: Rational = Rational(4,5)


     //------
     //TODO start here tomrorow

     //val rootC: _Root[Complex[Double], Double] = implicitly[_Root[Complex[Double], Double]]
     //println("NROOT TEST: " + rootC.nRoot(Complex(1.0, 2.0), 2.0))
     println("NROOT TEST: " + Complex(1.0, 2.0).nRoot(2.0))
     println("NROOT TEST: " + (Rational(2) ^ Rational(2)))
     println("ABS TEST: " + Real(-2).abs())

     //import linalg.syntax.AbsoluteSyntax._
     import scala.runtime.{RichInt => _, _}
     import scala.runtime.{ScalaNumberProxy => _, _}
     //println("ABS TEST: " + (-24).abs()) //todo this uses RichInt's abs method how to stop this?
     println("ABS TEST: " + Complex[Double](-1, 2).abs())

     /*import linalg.vector._
     import linalg.vector.VectorLike._
     import linalg.syntax.VectorLikeSyntax._
     import linalg.syntax.VectorSpaceSyntax._

     val v: Vector[Int] = Vector(1,2,3)
     println("VEC TEST: " + v.negate )
     println("VEC TEST: " + (v + v))
     v.negate()*/

     //------

     println(r1 + r2)
     println(c)

     println(b < c)
     println(b :==: c)
     println((4 + 3.i) :==: (4 + 3.i))
     println((2 + 5.i) < (2 + 7.i))
     println((2 + 5.i) < (2 - 5.i))
     println((8 + 2.i) + (9 + 2.i))
     println((8 + 2.i) - (9 + 2.i))
     println((8 + 2.i) < (9 + 2.i))

     println(a)
     println(b)
     println(a + Rational(1))
     println(Rational(33) + a)
     println(23.0 + (1.0 + 3.0.i))
     println((1.5 + 3.2.i) + 23.2)
     println((1 + 3.i) + 1)
     println(1 + (1 + 3.i))

     println(new Rational(4, 8))
     println(Rational(4, 8) + Rational(5, 15))
     println(Complex(1,2))
     println(Complex(1,2) + Complex(3,4))
}
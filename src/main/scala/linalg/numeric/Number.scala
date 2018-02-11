package linalg.numeric


import linalg.show.Show._
import linalg.theory._
import linalg.syntax.AbsoluteSyntax._
import linalg.syntax.CompareSyntax._
import linalg.syntax.NumberSyntax._
import linalg.syntax.RootSyntax._
import linalg.syntax.ShowSyntax._
import linalg.syntax.TrigSyntax._
import linalg.numeric.NumericConversion._


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


trait Compare[E] {
     def equal(x: E, y: E): Boolean
     def lessThan(x: E, y: E): Boolean
     def greaterThan(x: E, y: E): Boolean = lessThan(y, x)
     def lessThanOrEqual(x: E, y: E): Boolean = lessThan(x, y) || equal(x, y)
     def greaterThanOrEqual(x: E, y: E): Boolean = greaterThan(x, y) || equal(x, y)
}

trait Absolute0[H, L] {
     def absoluteValue(x: H): L
}

trait Absolute[A] extends Absolute0[A, A]

trait Root0[H, L] {
     val rOne: L
     val rTwo: L

     def power(base: H, exp: L): H
     def nRoot(base: H, n: L)(implicit div: Field[L]): H = power(base, div.divide(rOne, n))
     def squareRoot(base: H)(implicit f: Field[L]): H = nRoot(base, rTwo)
}

trait Root[R] extends Root0[R, R]

trait Trig[T] {

     val E: T
     val PI: T

     def sin(x: T): T
     def cos(x: T): T
     def tan(x: T): T
     def csc(x: T): T
     def sec(x: T): T
     def cot(x: T): T

     def arcsin(x: T): T
     def arccos(x: T): T
     def arctan(x: T): T
     def arccsc(x: T): T
     def arcsec(x: T): T
     def arccot(x: T): T

     //returns the theta component of polar (r, theta) of the x-y coordinate (x: T, y: T)
     def theta(y: T, x: T): T
}

//TODO WOW spire example here supports extending Number with Root etc instead of having them INSIDE like now
// https://insight.io/github.com/non/spire/blob/master/core/shared/src/main/scala/spire/std/bigInt.scala
//TODO revert back to previous work with separate typeclasses, no mixing with number, like similar to spire here:
//https://insight.io/github.com/non/spire/blob/master/core/shared/src/main/scala/spire/math/Complex.scala
//followup note: root doesn't have to know about number:
//note -  https://insight.io/github.com/non/spire/blob/master/core/shared/src/main/scala/spire/algebra/NRoot.scala
trait Number[N] extends Field[N] {
     val zero: N
     val one: N
     val two: N

     def plus(x: N, y: N): N
     def minus(x: N, y: N): N = plus(x, negate(y))
     def times(x: N, y: N): N
     def divide(x: N, y: N): N
     def negate(x: N): N
     def inverse(x: N): N
     def isZero(x: N): Boolean
     def isNegative(x: N): Boolean

     def doubleValue(x: N): Double
     def from(x: Int): N
}


trait RealLike[R] extends Number[R] {

     def from(x: Int): R
}





object Number {

     def ZERO[N](implicit gen: Number[N]): N = gen.zero
     def ONE[N](implicit gen: Number[N]): N = gen.one
     def TWO[N](implicit gen: Number[N]): N = gen.two



     //note: need to keep rr and pos as base 0 types not Root[R] since otherwise
     // note: the tests below don't work

     implicit def ComplexIsNumber[R: RealLike: Compare: Trig: Absolute: Root] = new Number[Complex[R]]
          with Compare[Complex[R]] with Root0[Complex[R],R] with Absolute0[Complex[R], R] with Trig[Complex[R]] {

          val realLike = implicitly[RealLike[R]]

          /** Number part */
          val zero: Complex[R] = Complex.ZERO[R]
          val one: Complex[R] = Complex.ONE[R]
          val two: Complex[R] = Complex.TWO[R]

          def plus(x: Complex[R], y: Complex[R]): Complex[R] = Complex(x.re + y.re, x.im + y.im)
          def times(x: Complex[R], y: Complex[R]): Complex[R] = Complex(x.re * y.im - y.re * x.im, x.re * y.re + y.im * x.im)
          def divide(x: Complex[R], y: Complex[R]): Complex[R] = {
               val prod: Complex[R] = times(x, y)
               val absDenom: R = Complex.magnitude(y)
               Complex(prod.re / absDenom, prod.im / absDenom)
          }
          def negate(x: Complex[R]): Complex[R] = Complex(x.re.negate(), x.im.negate())
          def isZero(x: Complex[R]): Boolean = equal(x, zero)
          def isNegative(x: Complex[R]): Boolean = x.re.isNegative && x.im.isNegative
          def isReal(x: Complex[R]): Boolean = x.im.isZero //todo make this visible in complex class or object.
          def isImaginary(x: Complex[R]): Boolean = !isReal(x)
          def doubleValue(x: Complex[R]): Double = Complex.magnitude(x).toDouble
          def from(x: Int): Complex[R] = Complex(realLike.from(x))

          /** Equality part */
          def equal(x: Complex[R], y: Complex[R]): Boolean = x.re :==: y.re && x.im :==: y.im
          def lessThan(x: Complex[R], y: Complex[R]): Boolean = x.re < y.re || (x.re :==: y.re && x.im < y.im)

          /** Root part */
          val rOne: R = realLike.one
          val rTwo: R = realLike.two
          def power(base: Complex[R], exp: R): Complex[R] =
               Complex(Complex.magnitude(base) ^ exp, Complex.angle(base) * exp)

          /** Absolute part */
          def absoluteValue(z: Complex[R]): R = Complex.magnitude(z)

          /** Trig part */
          val E: Complex[R] = ??? //Complex(scala.math.E).asInstanceOf[Complex[R]] //todo more graceful way?
          val PI: Complex[R] = ??? //Complex(scala.math.Pi).asInstanceOf[Complex[R]]

          //todo major todo
          def sin(x: Complex[R]): Complex[R] = ???
          def cos(x: Complex[R]): Complex[R] = ???
          def tan(x: Complex[R]): Complex[R] = ???
          def csc(x: Complex[R]): Complex[R] = ???
          def sec(x: Complex[R]): Complex[R] = ???
          def cot(x: Complex[R]): Complex[R] = ???

          def arcsin(x: Complex[R]): Complex[R] = ???
          def arccos(x: Complex[R]): Complex[R] = ???
          def arctan(x: Complex[R]): Complex[R] = ???
          def arccsc(x: Complex[R]): Complex[R] = ???
          def arcsec(x: Complex[R]): Complex[R] = ???
          def arccot(x: Complex[R]): Complex[R] = ???

          def theta(y: Complex[R], x: Complex[R]): Complex[R] = tan(divide(y, x))
     }



     implicit object RealIsNumber extends RealLike[Real] with Trig[Real] with Absolute[Real]
          with Root[Real] with Compare[Real] {

          val zero: Real = Real.ZERO
          val one: Real = Real.ONE
          val two: Real = Real.TWO


          def plus(x: Real, y: Real): Real = Real(x.double + y.double)
          def times(x: Real, y: Real): Real = Real(x.double * y.double)
          def divide(x: Real, y: Real): Real = Real(x.double / y.double)
          def negate(x: Real): Real = Real(-x.double)
          def isZero(x: Real): Boolean = equal(x, zero)
          def isNegative(x: Real): Boolean = x.double < 0
          def doubleValue(x: Real): Double = x.double
          def from(x: Int): Real = Real(x)

          /** Equality part */
          def equal(x: Real, y: Real): Boolean = x.double == y.double
          def lessThan(x: Real, y: Real): Boolean = x.double < y.double

          /** Root part */
          val rOne: Real = one
          val rTwo: Real = two
          def power(base: Real, exp: Real): Real = Real(math.pow(base.double, exp.double))

          /** Absolute value part */
          def absoluteValue(x: Real): Real = Real(math.abs(x.double))

          /** Trig part **/
          val E: Real = Real(scala.math.E)
          val PI: Real = Real(scala.math.Pi)

          def sin(x: Real): Real = Real(math.sin(x.double))
          def cos(x: Real): Real = Real(math.cos(x.double))
          def tan(x: Real): Real = Real(math.tan(x.double))
          def csc(x: Real): Real = divide(Real.ONE, sin(x))
          def sec(x: Real): Real = divide(Real.ONE, cos(x))
          def cot(x: Real): Real = divide(Real.ONE, tan(x))

          def arcsin(x: Real): Real = Real(math.asin(x.double))
          def arccos(x: Real): Real = Real(math.acos(x.double))
          def arctan(x: Real): Real = Real(math.atan(x.double))
          def arccsc(x: Real): Real = divide(Real.ONE, arcsin(x))
          def arcsec(x: Real): Real = divide(Real.ONE, arccos(x))
          def arccot(x: Real): Real = divide(Real.ONE, arctan(x))

          def theta(y: Real, x: Real): Real = Real(math.tan(y.double / x.double))
     }


     implicit object RationalIsRealNumber extends RealLike[Rational] with Trig[Rational] with Absolute[Rational]
          with Root[Rational] with Compare[Rational] {

          /** Real part */
          val zero: Rational = Rational.ONE
          val one: Rational = Rational.ONE
          val two: Rational = Rational.ONE

          def plus(x: Rational, y: Rational): Rational = Rational(x.num*y.den + y.num*x.den, x.den*y.den)
          def times(x: Rational, y: Rational): Rational = Rational(x.num * y.num, x.den * y.den)
          def divide(x: Rational, y: Rational): Rational = Rational(x.num * y.den, x.den * y.num)
          def negate(x: Rational): Rational = Rational(-x.num, -x.den)
          def isZero(x: Rational): Boolean = equal(x, zero)
          def isNegative(x: Rational): Boolean = x.num < 0
          def doubleValue(x: Rational): Double = x.num * 1.0 / x.den
          def from(x: Int): Rational = Rational(x)

          /** Root part */
          val rOne: Rational = one
          val rTwo: Rational = two
          def power(base: Rational, exp: Rational): Rational = Rational(math.pow(doubleValue(base), doubleValue(exp)))

          /** Absolute value part */
          def absoluteValue(x: Rational): Rational = Rational(math.abs(x.num), math.abs(x.den))

          /** Equality part **/
          def equal(x: Rational, y: Rational): Boolean = x.num * y.den == y.num * x.den
          def lessThan(x: Rational, y: Rational): Boolean = x.num * y.den < y.num * x.den

          /** Trig part **/
          val E: Rational = Rational(scala.math.E)
          val PI: Rational = Rational(scala.math.Pi) // this is what spire does too, because these are finit     e.

          def sin(x: Rational): Rational = Rational(math.sin(doubleValue(x)))
          def cos(x: Rational): Rational = Rational(math.cos(doubleValue(x)))
          def tan(x: Rational): Rational = Rational(math.tan(doubleValue(x)))
          def csc(x: Rational): Rational = divide(Rational.ONE, sin(x))
          def sec(x: Rational): Rational = divide(Rational.ONE, cos(x))
          def cot(x: Rational): Rational = divide(Rational.ONE, tan(x))

          def arcsin(x: Rational): Rational = Rational(math.asin(doubleValue(x)))
          def arccos(x: Rational): Rational = Rational(math.acos(doubleValue(x)))
          def arctan(x: Rational): Rational = Rational(math.atan(doubleValue(x)))
          def arccsc(x: Rational): Rational = divide(Rational.ONE, arcsin(x))
          def arcsec(x: Rational): Rational = divide(Rational.ONE, arccos(x))
          def arccot(x: Rational): Rational = divide(Rational.ONE, arctan(x))

          //todo isn't it supposed to be arctan?
          def theta(y: Rational, x: Rational): Rational = Rational(math.tan(doubleValue(x) / doubleValue(x)))
     }


     implicit object IntIsRealNumber extends RealLike[Int] with Trig[Int] with Absolute[Int]
          with Root[Int] with Compare[Int]{

          val zero: Int = 0
          val one: Int = 1
          val two: Int = 2

          def plus(x: Int, y: Int): Int = x + y
          def times(x: Int, y: Int): Int = x * y
          def divide(x: Int, y: Int): Int = x / y
          def negate(x: Int): Int = -x
          def isZero(x: Int): Boolean = x == 0
          def isNegative(x: Int): Boolean = x < 0
          def doubleValue(x: Int): Double = x * 1.0
          def from(x: Int): Int = x


          /** Equality part **/
          def equal(x: Int, y: Int): Boolean = x == y
          def lessThan(x: Int, y: Int): Boolean = x < y

          /** Absolute part */
          def absoluteValue(x: Int): Int = math.abs(x)

          /** Root part */
          val rOne: Int = 1
          val rTwo: Int = 2
          def power(base: Int, exp: Int): Int = math.pow(base, exp).toInt //not chopped off

          /** Trig part **/
          val E: Int = 2
          val PI: Int = 3 //just approximations! - note: int is not good for calculations

          def sin(x: Int): Int = math.sin(x).toInt
          def cos(x: Int): Int = math.cos(x).toInt
          def tan(x: Int): Int = math.tan(x).toInt
          def csc(x: Int): Int = (1.0 / sin(x)).toInt
          def sec(x: Int): Int = (1.0 / cos(x)).toInt
          def cot(x: Int): Int = (1.0 / tan(x)).toInt

          def arcsin(x: Int): Int = math.asin(x).toInt
          def arccos(x: Int): Int = math.acos(x).toInt
          def arctan(x: Int): Int = math.atan(x).toInt
          def arccsc(x: Int): Int = (1.0 / arcsin(x)).toInt
          def arcsec(x: Int): Int = (1.0 / arccos(x)).toInt
          def arccot(x: Int): Int = (1.0 / arctan(x)).toInt

          def theta(y: Int, x: Int): Int = math.tan(y / x).toInt
     }


     implicit object DoubleIsRealNumber extends RealLike[Double] with Trig[Double] with Absolute[Double]
          with Root[Double] with Compare[Double]{

          val zero: Double = 0.0
          val one: Double = 1.0
          val two: Double = 2.0

          def plus(x: Double, y: Double): Double = x + y
          def times(x: Double, y: Double): Double = x * y
          def divide(x: Double, y: Double): Double = x / y
          def negate(x: Double): Double = -x
          def isZero(x: Double): Boolean = x == 0
          def isNegative(x: Double): Boolean = x < 0
          def doubleValue(x: Double): Double = x
          def from(x: Int): Double = x * 1.0


          /** Equality part **/
          def equal(x: Double, y: Double): Boolean = x == y
          def lessThan(x: Double, y: Double): Boolean = x < y

          /** Absolute part */
          def absoluteValue(x: Double): Double = math.abs(x)

          /**Root part */
          val rOne: Double = 1.0
          val rTwo: Double = 2.0
          def power(base: Double, exp: Double): Double = math.pow(base, exp)

          /** Trig part **/
          val E: Double = scala.math.E
          val PI: Double = scala.math.Pi

          def sin(x: Double): Double = math.sin(x)
          def cos(x: Double): Double = math.cos(x)
          def tan(x: Double): Double = math.tan(x)
          def csc(x: Double): Double = 1.0 / sin(x)
          def sec(x: Double): Double = 1.0 / cos(x)
          def cot(x: Double): Double = 1.0 / tan(x)

          def arcsin(x: Double): Double = math.asin(x)
          def arccos(x: Double): Double = math.acos(x)
          def arctan(x: Double): Double = math.atan(x)
          def arccsc(x: Double): Double = 1.0 / arcsin(x)
          def arcsec(x: Double): Double = 1.0 / arccos(x)
          def arccot(x: Double): Double = 1.0 / arctan(x)

          def theta(y: Double, x: Double): Double = math.tan(y / x)
     }
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


case class Complex[R:RealLike](re:R, im:R) {
     override def toString: String = Complex(re, im).show
}


case class Imaginary[R: RealLike](im: R) {

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

     def ZERO[R](implicit gen: RealLike[R]): Complex[R] = new Complex(gen.zero, gen.zero)
     def ONE[R](implicit gen: RealLike[R]): Complex[R] = new Complex(gen.one, gen.zero)
     def TWO[R](implicit gen: RealLike[R]): Complex[R] = new Complex(gen.two, gen.zero)

     def apply[R](realPart: R)(implicit gen: RealLike[R]): Complex[R] = new Complex(realPart, gen.zero)


     // --- Operations ---
     def polar[R: RealLike](z: Complex[R])(implicit rr: Root[R], t: Trig[R]): Complex[R] =
          Complex(magnitude(z), angle(z))

     def magnitude[R: RealLike](z: Complex[R])(implicit rr: Root[R]): R =
          (z.re * z.re + z.im * z.im).sqrt()

     //just returns the value of theta for the complex number: theta = arctan(b / a), where c = a + bi
     def angle[R: RealLike](z: Complex[R])(implicit trig: Trig[R]): R = trig.theta(z.re, z.im)

     /** Returns the nth root of a complex number - in tuple form = (modulus root n, list of all roots) */
     def nthRootComplex[R](z: Complex[R], n: R)(implicit gen: RealLike[R],
                                                trig: Trig[R],
                                                rr: Root[R]): (R, List[R]) ={

          val two: R = gen.one + gen.one
          val polarComplex: Complex[R] = polar(z)
          val (modulus, theta): (R, R) = (polarComplex.re, polarComplex.im)

          val theNRoots: List[R] = List.tabulate[R](n.toInt)(k => (theta + two * gen.from(k) * trig.PI) / n)

          (modulus.nRoot(n), theNRoots)
     }

     def nthRootsOfUnity[R](z: Complex[R], n: R)(implicit gen: RealLike[R], trig: Trig[R]): List[R] = {
          val two: R = gen.one + gen.one
          List.tabulate[R](n.toInt)(k => (two * gen.from(k) * trig.PI) / n)
     }

     def conjugate[R: RealLike](z: Complex[R]): Complex[R] = Complex(z.re, z.im.negate())
}


object Imaginary {

     def ZERO[R](implicit gen: RealLike[R]): Imaginary[R] = new Imaginary(gen.zero)
     def ONE[R](implicit gen: RealLike[R]): Imaginary[R] = new Imaginary(gen.one)
     def TWO[R](implicit gen: RealLike[R]): Imaginary[R] = new Imaginary(gen.two)
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

     val rootC: Root0[Complex[Double], Double] = implicitly[Root0[Complex[Double], Double]]
     println("NROOT TEST: " + rootC.nRoot(Complex(1.0, 2.0), 2.0))
     println("NROOT TEST: " + Complex(1.0, 2.0).nRoot(2.0))
     println("NROOT TEST: " + (Rational(2) ^ Rational(2)))
     println("ABS TEST: " + Real(-2).abs)

     import linalg.syntax.AbsoluteSyntax._
     import scala.runtime.{RichInt => _, _}
     import scala.runtime.{ScalaNumberProxy => _, _}
     println("ABS TEST: " + (-23).abs) //todo this uses RichInt's abs method how to stop this?
     println("ABS TEST: " + Complex[Double](-1, 2).abs)

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
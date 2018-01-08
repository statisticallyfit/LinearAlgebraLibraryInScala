package linalg.numeric


import linalg.theory._
import linalg.util.Implicits._


import org.apache.commons.lang3.math.Fraction
import scala.language.implicitConversions

/**
  *
  *
  */


trait Number[T] {//extends Field[T] with Ordered[T] {

     //inherited: add, multiply, divide, one, zero, negate, inverse
//     val one: T
//     val zero: T

     def plus(x: T, y: T): T
     /*def minus(x: T, y: T): T
     def times(x: T, y: T): T
     def divide(x: T, y: T): T
     def power(x: T, y: T): T
     def squareRoot(x: T): T
     def absoluteValue(x: T): T

     def negate(x: T): T
     def inverse(x: T): T

     def isZero(x: T): Boolean
     def isNegative(x: T): Boolean
     def isEqual(x: T, y: T): Boolean
     //compare?

     def asDouble(x: T): Double*/
     def asString(x: T): String

     //note: just for complex: isreal, isimaginary, polar, rootsofunity, theta, conjugate ...
}


object ImplicitNumberOps {

     implicit class NumberOps[T](current: T)(implicit ev: Number[T]) {

          def +(other: T): T = ev.plus(current, other)
          /*def -(other: T): T = ev.minus(current, other)
          def *(other: T): T = ev.times(current, other)
          def /(other: T): T = ev.divide(current, other)
          def ^(expo: T): T = ev.power(current, expo)

          def sqrt(): T = ev.squareRoot(current)
          def abs(): T = ev.absoluteValue(current)

          def negate(): T = ev.negate(current)
          def inverse(): T = ev.inverse(current)

          def isZero: Boolean = ev.isZero(current)
          def isNegative: Boolean = ev.isNegative(current)
          def ==(other: T): Boolean = ev.isEqual(current, other)

          def toDouble: Double = ev.asDouble(current)*/
          override def toString: String = ev.asString(current)
     }
}




case class Complex(re: Rational, im: Rational)

case class Real(value: Double){
     //override def toString: String = value.toString //todo why tostring doesn't work in NumberOps?
}

case class Rational(num: Int, denom: Int)

case class Natural(value: Int) { require(value > 0) }



object Number {

     implicit object IntNumber extends Number[Int] {
          def plus(x: Int, y: Int): Int = x + y
          def asString(x: Int): String = x.toString
     }

     implicit object DoubleNumber extends Number[Double] {
          def plus(x: Double, y: Double): Double = x + y
          def asString(x: Double): String = x.toString
     }

     implicit object BigDecimalNumber extends Number[BigDecimal] {
          def plus(x: BigDecimal, y: BigDecimal): BigDecimal = x + y
          def asString(x: BigDecimal): String = x.toString
     }

     // Now my own types: ------------------------------------------------
     implicit val ComplexNumber = new Number[Complex] {

          def plus(x: Complex, y: Complex): Complex = Complex(x.re) //todo

          def asString(x: Complex): String = x.re + " + " + x.im + "i"//todo fix later
     }

     implicit val RealNumber = new Number[Real] {
          def plus(x: Real, y: Real): Real = Real(x.value + y.value)
          def asString(x: Real): String = x.value.toString
     }

     implicit val RationalNumber = new Number[Rational] {
          //val f: Fraction = Fraction.getFraction()
          def plus(x: Rational, y: Rational): Rational =
               Rational(x.num*y.denom + y.num*x.denom, x.denom * y.denom)

          def asString(x: Rational): String = x.num + " / " + x.denom //todo fix later
     }

     implicit val NaturalNumber = new Number[Natural] {
          def plus(x: Natural, y: Natural): Natural = Natural(x.value + y.value)

          def asString(x: Natural): String = x.value.toString
     }

     //---------------------------------------------------------------------
     //todo - why doesn't this nor asString method work?
     /*implicit class ComplexString(val complex: Complex) {
          override def toString: String = complex.real + " + " + complex.imag + "i"
     }
     implicit class RealString(val real: Real) {
          override def toString: String = real.value.toString
     }*/
}



object NumberTester extends App {
     import ImplicitNumberOps._

     /*val c1: Complex[Real] = Complex(Real(1), Real(3))
     val c2: Complex[Real] = Complex(Real(2), Real(5))
     println(c1)*/
     val c1: Complex[Int] = Complex(1,2)

     println(Real(23))
     println(Real(1) + Real(1))
     println()
     //println(Complex(1, 2) + Complex(3, 4))

}


//todo here tomorrow: http://blog.kamkor.me/Covariance-And-Contravariance-In-Scala/

// Number trait does nothing - just here to look pretty and get the "nice"
// methods from Field: +, *, ...
//trait Number[N <: Number[N]] extends Field[N] with Ordered[N] { this: N =>
//     //inherited: +, *, /, inverse, negate, one, zero
//
//     def one: N
//     def zero: N
//
//     def +(other: N): N
//     def -(other: N): N
//     def *(other: N): N
//     def /(other: N): N
//     def ^(exp: N): N
//
//     def abs(): N
//     def sqrt(): N
//
//     def negate(): N
//     def inverse(): N
//
//     def ==(that: N): Boolean
//     def compare(that: N): Int
//
//     def isZero: Boolean
//     def isNegative: Boolean
//     def isReal: Boolean
//     def isImaginary: Boolean
//
//     def toDouble: Double
//     def toString: String
//}
//
//object Number {
//     def ZERO[N <: Number[N]](implicit n: Number[N]): N = n.zero
//     def ONE[N <: Number[N]](implicit n: Number[N]): N = n.one
//     def TWO[N <: Number[N]](implicit n: Number[N]): N = n.one + n.one //todo: to work, put N <: Number[N]
//     /*def ONE[N <: Number[N]](implicit n: Number[N]): N = n.one
//     def TWO[N <: Number[N]](implicit n: Number[N]): N = n.one + n.one*/
//}
//
//
//
////private[numeric] trait Numeric[N <: Numeric[N]] extends Number[Numeric[N]]
//
//
///*(implicit n: Number[N])*/
//class Complex[N <: Number[N]/*: Complex*/](val re: N, val im: N) extends Number[Complex[N]] {
//
//     implicit val n: Number[N] = implicitly[Number[N]]
//
//     private val modulus: Double = abs().toDouble
//     //math.sqrt(math.pow(re.toDouble, 2) + math.pow(im.toDouble, 2))
//
//     //def zero: N = Number.ZERO[N]
//     val zero: Complex[N] = Complex.ZERO[N]
//     //def one: N = Number.ONE[N]
//     val one: Complex[N] = Complex.ONE[N] //todo which one???? above or this one? both types work ....!
//
//
//     def +(other: Complex[N]): Complex[N] = Complex(re + other.re, im + other.im)
//     def -(other: Complex[N]): Complex[N] = Complex[N](re - other.re, im - other.im)
//     def *(other: Complex[N]): Complex[N] = Complex[N](re * other.im - other.re * im, re * other.re + other.im * im)
//
//     def /(other: Complex[N]): Complex[N] = {
//          val newNumerator: Complex[N] = this * other
//          val newDenominator: N = other.abs().re
//          //todo
//          assert(other.abs().im.isZero)
//
//          Complex(re / newDenominator, im / newDenominator)
//     }
//
//     def ^(exp: Complex[N]): Complex[N] = ???
//
//     def abs(): Complex[N] =  Complex((re^Number.TWO[N] + im^Number.TWO[N]).sqrt(), Number.ZERO[N])
//     //Complex[N] = Complex((re^Number.TWO[N] + im^Number.TWO[N]).sqrt(), Number.ZERO[N])
//     def sqrt(): Complex[N] = ??? //call the roots of unity function with 1/2 as arg (make it have a Real arg)
//
//     def negate(): Complex[N] = Complex(re.negate(), im.negate())
//     def inverse(): Complex[N] = Complex.ZERO[N] / this
//
//     def ==(other: Complex[N]): Boolean = re == other.re && im == other.im
//     def compare(other: Complex[N]): Int = (this - other).toDouble.toInt
//
//     def isZero: Boolean = re.isZero && im.isZero
//     def isNegative: Boolean = re.isNegative && im.isNegative
//     def isReal: Boolean = im.isZero
//     def isImaginary: Boolean = !isReal
//
//     def toDouble: Double = modulus
//
//     /**
//       * Complex Number logic here
//       */
//     def conjugate(): Complex[N] = Complex(re, im.negate())
//     //def polar(): Polar[N] = ???
//     //def rootsOfUnity(): Complex[N] = ???
//     //todo  need to normalize for theta to be between -pi and pi?
//     //def theta(): Double = ??? //math.atan((im / re).toDouble) //todo: make a Trig trait with all trig functions ...
//
//
//     override def toString: String = {
//
//          val genZero: N = Number.ZERO[N]
//          val genOne: N = Number.ONE[N]
//
//          val stringComplex: String = this match {
//               //todo case Complex.i => "i"
//               case Complex(real, genZero) => real.toString
//               case Complex(genOne, imaginary) => imaginary.toString + "i"
//               case _ => {
//                    val imStr: String = if(im < genZero) " - " + im.negate() else " + " + im + "i"
//                    re.toString + imStr
//               }
//          }
//
//          stringComplex
//     }
//}
//
//
//// note: the new thing: here I am just copying the methods from Numerical trait
//// using alternate object syntax for sake of prettiness.
///*(implicit n: Number[Real])*/
//class Real(val value: Double) extends Complex[Real](value, 0)


     /** As seen from class Real, the missing signatures are as follows.
       *  For convenience, these are usable as stub implementations.
       */
     /*def /(other: linalg.numeric.Real): linalg.numeric.Real = ???
     def ==(that: linalg.numeric.Real): Boolean = ???
     def -(other: linalg.numeric.Real): linalg.numeric.Real = ???
     def +(other: linalg.numeric.Real): linalg.numeric.Real = ???
     def *(other: linalg.numeric.Real): linalg.numeric.Real = ???
     def ^(exp: linalg.numeric.Real): linalg.numeric.Real = ???
     def abs(): linalg.numeric.Real = ???
     def compare(that: linalg.numeric.Real): Int = ???
     def inverse(): linalg.numeric.Real = ???
     def isImaginary: Boolean = ???
     def isNegative: Boolean = ???
     def isReal: Boolean = ???
     def isZero: Boolean = ???
     def negate(): linalg.numeric.Real = ???
     def one: linalg.numeric.Real = ???
     def sqrt(): linalg.numeric.Real = ???
     def toDouble: Double = ???
     def zero: linalg.numeric.Real = ???*/
     //class Real(val value: Double) extends Number[Real] //Complex[Real](Real(value), Real(0))

//class Rational(val num: Int, val denom: Int)/*(implicit n: Number[Real])*/ extends Real(num * 1.0 / denom)  {
//     override def toString: String = this.denom match {
//          case 0 => num.toString
//          case _ => num + " / " + denom
//     }
//}
//
//class Natural(value: Int)/*(implicit n: Number[Real])*/ extends Rational(value, 1) { require(value > 0) }


// ---------------------

//object Real {
//
//     /*def ZERO[N](implicit n: Number[Real]): Real = new Real(0)
//     def ONE[N](implicit n: Number[Real]): Real = new Real(1)*/
//     val ZERO: Real = new Real(0)
//     val ONE: Real = new Real(1)
//
//     def apply(doubleValue: Double)/*(implicit n: Number[Real])*/ = new Real(doubleValue)
//     def unapply(real: Real)/*(implicit n: Number[Real])*/: Option[Double] = Some(real.value)
//
//     implicit def doubleToReal(d: Double)/*(implicit n: Number[Real])*/: Real = new Real(d)
//     implicit def intToReal(i: Int)/*(implicit n: Number[Real])*/: Real = new Real(i)
//}
//
//
////object i extends Complex[Int](0, 1)
//object Complex {
//
//
//     //todo use above object? else how to get in the type parameter?
//     def i[N <: Number[N]](implicit n: Number[N]): Complex[N] = Complex(n.zero, n.one)
//
//     def ONE[N <: Number[N]](implicit n: Number[N]): Complex[N] = Complex(n.one, n.zero)
//     def ZERO[N <: Number[N]](implicit n: Number[N]): Complex[N] = Complex(n.zero, n.zero)
//
//     def apply[N <: Number[N]](re: N, im: N)(implicit n: Number[N]) = new Complex[N](re, im)
//     def unapply[N <: Number[N]](complex: Complex[N])(implicit n: Number[N]): Option[(N, N)] =
//          Some(complex.re, complex.im)
//
//
//     //todo
//     /*implicit def doubleToComplex(d: Double)(implicit n: Number[Real]): Complex[Real] =
//          new Complex(Real(d), Real(0))*/
//
//     /*implicit def intToComplex(i: Int)(implicit n: Number[Natural]): Complex[Natural] =
//          new Complex(Natural(i), Natural(0))*/
//}


//object Natural {
//
//     //val n = implicitly[Number[Natural]]
//     val ZERO: Natural = new Natural(0)
//     val ONE: Natural = new Natural(1)
//
//     def apply(intValue: Int) = new Natural(intValue)
//     def unapply(natural: Natural): Option[Int] = Some(natural.value.toInt)
//
//     implicit def intToNatural(i: Int): Natural = new Natural(i)
//}
//
//object Rational {
//     val ZERO: Rational = new Rational(0, 1)
//     val ONE: Rational = new Rational(1, 1)
//
//     def apply(numerator: Int, denominator: Int) = new Rational(numerator, denominator)
//     def unapply(rational: Rational): Option[(Int, Int)] = Some(rational.num, rational.denom)
//}










// -------------------------------------------------------------------------





//note: all the non-parameter stuff - not working because Scala doesn't have regular
// polymorphism and expects the actual Number type, not subtypes of it (Number won't resolve as subtypes)




/*implicit nb: NumericBase[N],
implicit val c: NumericBase[Complex[N]],
implicit val n: Number[N]*/


/*
class Complex[N <: Number[N]](val re: N, val im: Number)/*(implicit n: Number)*/ extends Number {


//     private val modulus: Double = abs().toDouble
//          //math.sqrt(math.pow(re.toDouble, 2) + math.pow(im.toDouble, 2))
//
//     //def zero: N = Number.ZERO[N]
//     protected def zero: Number = Complex.ZERO
//     //def one: N = Number.ONE[N]
//     protected def one: Number = Complex.ONE //todo which one???? above or this one? both types work ....!


     def +(other: Complex): Complex = new Complex(re + other.re, im + other.im)
     def -(other: Complex): Number = new Complex(re - other.re, im - other.im)
//     def *(other: Complex): Complex = Complex(re * other.im - other.re * im, re * other.re + other.im * im)
//
//     def /(other: Complex): Complex = {
//          val newNumerator: Complex = this * other
//          val newDenominator: Number = other.abs()
//
//          Complex(re / newDenominator, im / newDenominator)
//     }
//
//     def ^(exp: Number): Complex = ???
//
//     //note - can also have return type 'N' because N <: Number[N].
//     def abs(): Number =  (re^Number.TWO + im^Number.TWO).sqrt()
//     //Complex[N] = Complex((re^Number.TWO[N] + im^Number.TWO[N]).sqrt(), Number.ZERO[N])
//     def sqrt(): Number = ??? //call the roots of unity function with 1/2 as arg (make it have a Real arg)
//
//     def negate(): Complex = Complex(re.negate(), im.negate())
//     def inverse(): Complex = Complex.ZERO / this
//
//     def ==(other: Complex): Boolean = re == other.re && im == other.im
     def compare(other: Number): Int = (this - other).toDouble.toInt
//
//     def isZero: Boolean = re.isZero && im.isZero
//     def isNegative: Boolean = re.isNegative && im.isNegative
//     def isReal: Boolean = im.isZero
//     def isImaginary: Boolean = !isReal
//
//     def toDouble: Double = modulus
//
//     /**
//       * Complex Number logic here
//       */
//     def conjugate(): Complex = Complex(re, im.negate())
//     //def polar(): Polar[N] = ???
//     //def rootsOfUnity(): Complex[N] = ???
//     //todo  need to normalize for theta to be between -pi and pi?
//     //def theta(): Double = ??? //math.atan((im / re).toDouble) //todo: make a Trig trait with all trig functions ...
//
//
//     override def toString: String = {
//
//          val genZero: Number = Number.ZERO
//          val genOne: Number = Number.ONE
//
//          val stringComplex: String = this match {
//               //todo case Complex.i => "i"
//               case Complex(real, genZero) => real.toString
//               case Complex(genOne, imaginary) => imaginary.toString + "i"
//               case _ => {
//                    val imStr: String = if(im < genZero) " - " + im.negate() else " + " + im + "i"
//                    re.toString + imStr
//               }
//          }
//
//          stringComplex
//     }
     //todo old code
     /* val realTemp: Rational = Rational(real)
     val imagTemp: Rational = Rational(imaginary)

     if(realTemp.isZero && imagTemp.isZero) return "0"
     if(imagTemp.isZero) return realTemp.toString

     var imagStr: String = ""
     var realStr: String = ""

     if(realTemp.isZero){
          //dealing with imag now
          imagStr = if(imagTemp == -1) "-i"
          else if(imagTemp == 1) "i"
          else if(imagTemp.isNegative) imagTemp + "i"
          else imagTemp + "i"
     } else {
          realStr = realTemp.toString
          imagStr = if(imagTemp == -1) " - i"
          else if(imagTemp == 1) " + i"
          else if(imagTemp.isNegative) " - " + imagTemp.abs() + "i"
          else " + " + imagTemp + "i"
     }

     realStr + imagStr
       */
}*/


// note: the new thing: here I am just copying the methods from Numerical trait
// using alternate object syntax for sake of prettiness.
//class Real(val value: Double) extends Complex(Real(value), Real(0))
//
//class Rational(val num: Int, val denom: Int) extends Real(num * 1.0 / denom)  {
//     override def toString: String = this.denom match {
//          case 0 => num.toString
//          case _ => num + " / " + denom
//     }
//}
//
//class Natural(value: Int) extends Rational(value, 1) { require(value > 0) }
//
//
//// ---------------------
//
//object Real {
//
//     val ZERO: Real = new Real(0)
//     val ONE: Real = new Real(1)
//     /*val ZERO: Real = new Real(0)
//     val ONE: Real = new Real(1)*/
//
//     def apply(doubleValue: Double) = new Real(doubleValue)
//     def unapply(real: Real): Option[Double] = Some(real.value)
//
//     implicit def doubleToReal(d: Double): Real = new Real(d)
//     implicit def intToReal(i: Int): Real = new Real(i)
//}
//
//
////object i extends Complex[Int](0, 1)
//object i extends Complex(0, 1) // todo make type class instance for Int //
//object Complex {
//
//     //todo use above object? else how to get in the type parameter?
//     def I(implicit n: Number): Complex = Complex(n.zero, n.one)
//
//     val ONE: Complex = Complex(Number.ONE, Number.ZERO) //or need implicit and use as below?
//          //def ONE(implicit n: Number): Complex = Complex(n.one, n.zero)
//     val ZERO: Complex = Complex(Number.ZERO, Number.ZERO)
//
//     def apply(re: Number, im: Number) = new Complex(re, im)
//     def unapply(complex: Complex): Option[(Number, Number)] = Some(complex.re, complex.im)
//
//
//     //todo
//     /*implicit def doubleToComplex(d: Double)(implicit n: Number[Real]): Complex[Real] =
//          new Complex(Real(d), Real(0))*/
//
//     /*implicit def intToComplex(i: Int)(implicit n: Number[Natural]): Complex[Natural] =
//          new Complex(Natural(i), Natural(0))*/
//}
//
//
//object Natural {
//
//     val n = implicitly[Number[Natural]]
//     val ZERO: Natural = new Natural(0)
//     val ONE: Natural = new Natural(1)
//
//     def apply(intValue: Int) = new Natural(intValue)
//     def unapply(natural: Natural): Option[Int] = Some(natural.value.toInt)
//
//     implicit def intToNatural(i: Int): Natural = new Natural(i)
//}
//
//object Rational {
//     val ZERO: Rational = new Rational(0, 1)
//     val ONE: Rational = new Rational(1, 1)
//
//     def apply(numerator: Int, denominator: Int) = new Rational(numerator, denominator)
//     def unapply(rational: Rational): Option[(Int, Int)] = Some(rational.num, rational.denom)
//}




//object Tester extends App {
//     println(Real.ZERO)
//     println(Real(31))
//     println(Real(31).negate())
//     println(Real(24) + Real(31).negate())
//     /*import Numerical._
//
//     def addTwoNumbers[A](first: A, second: A)(implicit n: Numerical[A]): A = {
//          n.plus(first, second)
//     }
//
//
//
//     Console.println(addTwoNumbers(Real(24), Real(31)))*/
//}


// ---------------------------------------------------------------------------------







// ----------------------------------------
//numerical trait is just here to provide implementation
/*private[linalg] trait NumericBase[N] { //note finally, intermediary type to do grunt work!

     //val baseOne: N
     //val baseZero: N //todo rename these? seems ugly to have one and zero in both Number and NumericBase ...???

     def plus(x: N, y: N): N
     def minus(x: N, y: N): N
     def times(x: N, y: N): N
     def divide(x: N, y: N): N
     def power(x: N, y: N): N
     def squareRoot(x: N): Double
     def absoluteValue(x: N): Double

     def negate(x: N): N

     def inverse(x: N): N

     def isZero(x: N): Boolean
     def isNegative(x: N): Boolean
     def isEqual(x: N, y: N): Boolean

     def toDouble(x: N): Double
}*/


/**
  * stuff that was in Complex class when using NumericBase typeclass:
  *
  *

/*def +(other: Complex[N]): Complex[N] = c.plus(this, other)
     def -(other: Complex[N]): Complex[N] = c.minus(this, other)
     def *(other: Complex[N]): Complex[N] = c.times(this, other)
     def /(other: Complex[N]): Complex[N] = c.divide(this, other)
     //def (exp: Complex[N]): Complex[N] = c.power(this, exp)

     //def something = re + im //works when I have N "lessthansign": Number[N] in type.

     def sqrt(): Double = c.squareRoot(this)
     def abs(): Double = c.absoluteValue(this)

     def conjugate(): Complex[N] = Complex(re, nb.negate(im))

     def negate(): Complex[N] = c.negate(this)
     def inverse(): Complex[N] = c.inverse(this)

     def isZero: Boolean = c.isZero(this)
     def isNegative: Boolean = c.isNegative(this)
     override def compare(that: Complex[N]): Int = (this.toDouble - that.toDouble).toInt
     def ==(that: Complex[N]): Boolean = c.isEqual(this, that)*/




  stuff that was in Real class when using NumericBase typeclass:

  // Protected because don't want to use these outside of class
     // Placed here to satisfy Field and Ring
     // Instead I want to use ZERO and ONE from the static Object.
     /*protected def zero: Real = new Real(0)
     protected def one: Real = new Real(1)

     def +(other: Real): Real = n.plus(this, other)
     def -(other: Real): Real = n.minus(this, other)
     def *(other: Real): Real = n.times(this, other)
     def /(other: Real): Real = n.divide(this, other)
     def (exp: Real): Real = n.power(this, exp)
     def sqrt(): Double = n.squareRoot(this)*/
     //def abs(): Double = n.absoluteValue(this)

     //def negate(): Real = n.negate(this)

     //def inverse(): Real = n.inverse(this)

     //def isZero: Boolean = n.isZero(this)
     //def isNegative: Boolean = n.isNegative(this)

     //todo: cannot make Numerical Ordered because compare() only takes one argument ..
     //override def compare(that: Real): Int = (this - that).toInt
     //def equals(that: Real): Boolean = this.value == that.value

     //override def toString: String = value.toString


  */



// this is standard typeclass pattern
/*object NumericBase {

     /*implicit object RealIsNumeric extends NumericBase[Real] {
          //override def nOne = Real(1)
          //override def numericalZero = Real(0)

          def plus(x: Real, y: Real): Real = Real(x.value + y.value)
          def minus(x: Real, y: Real): Real = Real(x.value - y.value)
          def times(x: Real, y: Real): Real = Real(x.value * y.value)
          def divide(x: Real, y: Real): Real = Real(x.value / y.value)
          def power(base: Real, exp: Real): Real = Real(scala.math.pow(base.value, exp.value))
          def squareRoot(x: Real): Double = scala.math.sqrt(x.value)
          def absoluteValue(x: Real): Double = scala.math.abs(x.value)

          def negate(x: Real): Real = Real(-x.value)

          def inverse(x: Real): Real = divide(Real.ONE, x)

          def isZero(x: Real): Boolean = x equals Real.ZERO
          def isNegative(x: Real): Boolean = x < Real.ZERO

          def toDouble(x: Real) = x.value

     }*/


     implicit def ComplexIsNumeric[N](implicit b: NumericBase[N]) = new NumericBase[Complex[N]] {

          type C = Complex[N]

          def plus(x: C, y: C): C = Complex(b.plus(x.re, y.re), b.plus(x.im, y.im))

          def minus(x: C, y: C): C = Complex(b.minus(x.re, y.re), b.minus(x.im, y.im))

          def times(x: C, y: C): C = Complex(b.minus(b.times(x.re, y.im), b.times(y.re, x.im)),
               b.plus(b.times(x.re, y.re), b.times(y.im, x.im)))

          def divide(x: C, y: C): C = ???/*{

               //val two: N = n.plus(n.nOne, n.nOne)

               val quot: Complex[N] = times(x, y.conjugate())
               val recMod: Double = math.pow(n.toDouble(y.re), 2) + math.pow(n.toDouble(y.im), 2)
                    //n.plus(n.power(y.re, two),  n.power(y.im, two))

               Complex[N](Real(n.toDouble(quot.re) / recMod), n.toDouble(quot.im) / recMod)
          }*/

          def power(base: C, exp: C): C = ???

          def squareRoot(x: C): C = ???

          // todo polar, rootsOfUnity, theta ,...

          def absoluteValue(x: C): Double = math.pow(b.toDouble(x.re), 2) + math.pow(b.toDouble(x.im), 2)

          def negate(x: C): C = Complex(b.negate(x.re), b.negate(x.im))

          def inverse(x: C): C = divide(Complex.ONE[N], x)

          //def conjugate(x: C): C = Complex(x.re, n.negate(x.im)) //todo erase from here since NumericBase
          // isn't necessarily a complex number. If we put it here, then it must go in numericbase - no!

          def isZero(x: C): Boolean = isEqual(x, Complex.ZERO[N])
          def isNegative(x: C): Boolean  = b.isNegative(x.re) && b.isNegative(x.im)
          def isEqual(x: C, y: C): Boolean = x.re == y.re && x.im == y.im

          def toDouble(x: C): Double = absoluteValue(x)

     }

}*/


// --------------------------------------




/*sealed trait Number[T <: Numeric[T]] extends Ring[T] with Field[T] with Ordered[T]{
     //inherited: ONE, ZERO, +, -, *, /, inverse, compare, equals
     def negate(): T
     def abs(): Real
     def ^(exp: T): T
     def sqrt(): Real

     def isZero: Boolean
     def isNegative: Boolean

     def plus(a: T, b: T): T
}

/*object Number {
     def plus[A](x: A, y: A)(implicit numeric: Numeric[A]): A = numeric.plus(x, y)

}*/
trait FakeNum[T] {
     def plus(a: T, b: T): T
}

object FakeNum {
     implicit object RealIsNumeric extends FakeNum[Real]{
          def plus(x: Real, y: Real): Real = Real(x.dec + y.dec)
     }
}

object Temp {
     def makeRealsAdd[N](a: N, b: N)(implicit n: FakeNum[N]) = n.plus(a, b)
}

class Real(val dec: Double)(implicit n: FakeNum[Real]) /*{
     def +(other: Real) = n.plus(this, other)
}*/
object Real {
     implicit def apply(real: Double)(implicit n : Numeric[Real]): Real = new Real(real)(n)
}


object NumberTester extends App {

     val r1 = new Real(3)
     val r2 = new Real(5)
     println(r1 + r2 + r1)
     /*val c1: Complex[Int] = Complex(3, 2)
     val c2: Complex[Int] = Complex(5, 1)
     //println(c1 plus c2)
     println(c1.plus(c1, c2))*/
}*/

/*class Complex[N <: Numeric[N]](val re: N, val im: N) extends NumFake[N]{
     def +(other: Complex[N]): Complex[N] = new Complex(re + other.re, im + other.im)
}*/

/*object Number {

     //implicit def intToComplex(int: Int): Complex[Int] = new Complex(int, 0)
     //implicit def doubleToComplex(double: Double): Complex[Double] = new Complex(double, 0)

     implicit class IntOps(int: Int) extends Number[Int] {
          override val ONE: Int = 1
          override val ZERO: Int = 0

          def plus(a: Int, b: Int) = a + b

          def +(other: Int): Int = int + other
          def *(other: Int): Int = int * other
          def ^(exp: Int): Int = int ^ exp
          def /(other: Int): Int = int / other
          def sqrt(): Double = scala.math.sqrt(int)

          def abs(): Double = if(int < 0) int.negate()*1.0 else int*1.0

          def inverse(): Int = int.negate()

          def negate(): Int = int * -1 //Option[Int] = None

          def compare(other: Int): Int = other + int.inverse()

          def isNegative: Boolean = int < 0
          def isZero: Boolean = int == 0
     }

     implicit class DoubleOps(double: Double) extends Number[Double] {
          override val ONE: Double = 1
          override val ZERO: Double = 0

          def plus(a: Double, b: Double) = a + b

          def +(other: Double): Double = double + other
          def *(other: Double): Double = double * other
          def ^(exp: Double): Double = double ^ exp
          def /(other: Double): Double = double / other
          def sqrt(): Double = scala.math.sqrt(double)

          def abs(): Double = if(double < 0) double.negate() else double

          def inverse(): Double = double.negate()

          def negate(): Double = double * -1 //Option[Int] = None

          def compare(other: Double): Int = (double - other).toInt

          def isNegative: Boolean = double < 0
          def isZero: Boolean = double == 0
     }
}*/

/*class Complex[N](val re: N, val im: N)(implicit n: Number[N]) /*extends Number[Complex[N]]*/ {

     val ZERO: Complex[N] = Complex(n.ZERO, n.ZERO)
     val ONE: Complex[N] = Complex(n.ONE, n.ZERO)

     //def +(other: Complex[N]): Complex[N] = new Complex[N](n + other.re, other.im)
     // .im)
     //def -(other: Complex[N]): Complex[N] = Complex(n  )
     def plus(a: N, b: N): N = n.plus(a, b)
}

object Complex {

     implicit def apply[N](re: N, im: N)(implicit n: Number[N]) = new Complex[N](re, im)(n)

     implicit def apply(int: Int)(implicit n: Number[Int]): Complex[Int] =  new Complex[Int](int, 0)(n)

     implicit def apply(double: Double)(implicit n: Number[Double]): Complex[Double] =  new Complex[Double](double, 0)(n)
}*/









//------------------------------------------------------------------------------------------------------

/*case class Complex(re: Int, im: Int) extends Number[Complex] { //(implicit n: Number[Complex])//
     val ZERO: Complex = Complex(0, 0)
     val ONE: Complex = Complex(1, 0)

     def +(that: Complex): Complex = Complex(re + that.re, im + that.im)
     def -(that: Complex): Complex = Complex(re - that.re, im - that.im)
     def *(that: Complex): Complex = Complex(re*that.im - that.re*im, re*that.re + that.im*im)
     def ^(power: )
     def /(that: Complex): Complex = {
          //if (b.isZero) throw new ArithmeticException
          val quot: Complex = this * that.conjugate()
          val recMod: Double = that.re ^ 2 + that.im ^ 2
          new Complex(quot.re/recMod, quot.im/recMod)
     }
     def conjugate(): Complex = Complex(re, -im)

}*/






/*object Complex {
     implicit def apply(n: Int): Complex[Int] = new Complex(n)
}*/

/*
object Number {

     implicit val complexNumber = new Number[Complex[Int]] {

          /*def /(a: Complex[Int], b: Complex[Int]): Complex[Int] = {
               //if (b.isZero) throw new ArithmeticException
               val quot: Complex[Int] = a * conjugate(b)
               val recMod: Real = (that.real ^ 2) + (that.imaginary ^ 2)
               new Complex(quot.real/recMod, quot.imaginary/recMod)
          }*/
          //todo def ^(a: Complex[Int], b: Complex[Int]): Complex[Int]
          //def sqrt(c: Complex[Int]): Real

          def inverse(c: Complex[Int]): Complex[Int] = conjugate(c)
          def abs(c: Complex[Int]): Real = Real(0) //todo
          //todo def abs(c: Complex[Int]): Real = Real((c.re^2 + c.im^2).sqrt())

          def conjugate(c: Complex[Int]): Complex[Int] = Complex(c.re, -c.im)

          /*todo def compareHelper(a: Complex[Int], b: Complex[Int]): Int = abs(a) - abs(b)
          def compare(other: Complex[Int]): Int = compareHelper(this, other)*/

          def areEqual(a: Complex[Int], b: Complex[Int]): Boolean = a.re == b.re && a.im == b.im

     }
}*/

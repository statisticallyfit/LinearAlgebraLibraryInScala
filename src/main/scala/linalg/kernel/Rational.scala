package linalg.kernel

import org.apache.commons.lang3.math.Fraction

import linalg.implicits._
/**
  *
  */

case class Rational(private val n: Int, private val d: Int) {
     val reduced: Fraction = Fraction.getFraction(n, d).reduce()
     val num: Int = reduced.getNumerator
     val den: Int = reduced.getDenominator

     override def toString: String = Rational(num, den).show
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

package linalg

import linalg.kernel.{Rational, Real}
import scala.language.implicitConversions

/**
  *
  */
object implicits extends syntax.AllSyntax with instances.AllInstances with GeneralImplicits

trait GeneralImplicits {
     //Converting Int to Rational
     implicit def intToRational(int: Int): Rational = Rational(int)
     // Converting Double To Real
     implicit def doubleToReal(double: Double): Real = Real(double)
}
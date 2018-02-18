package linalg.kernel

import linalg.implicits._

/**
  *
  */
case class Real(double: Double) {
     override def toString = Show[Real].show(this)
}

object Real {
     val ZERO: Real = new Real(0)
     val ONE: Real = new Real(1)
     val TWO: Real = new Real(2)
}
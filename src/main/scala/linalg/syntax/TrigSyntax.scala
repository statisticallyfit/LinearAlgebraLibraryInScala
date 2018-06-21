package linalg.syntax

import linalg._

import scala.language.higherKinds
import scala.language.implicitConversions


/**
  *
  */
trait TrigSyntax {

     implicit class TrigOps[T: Trig:Field](current: T){

          private val trig: Trig[T] = implicitly[Trig[T]]

          // Trig stuff
          def sin(): T = trig.sin(current)
          def cos(): T = trig.cos(current)
          def tan(): T = trig.tan(current)
          def csc(): T = trig.csc(current)
          def sec(): T = trig.sec(current)
          def cot(): T = trig.cot(current)
          def arcsin(): T = trig.arcsin(current)
          def arccos(): T = trig.arccos(current)
          def arctan(): T = trig.arctan(current)
          def arccsc(): T = trig.arccsc(current)
          def arcsec(): T = trig.arcsec(current)
          def arccot(): T = trig.arccot(current)
          def theta(x: T): T = trig.theta(current, x)
     }
}

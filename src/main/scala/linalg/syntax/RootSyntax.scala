package linalg.syntax

import linalg.implicits._
import linalg._

import scala.language.higherKinds
import scala.language.implicitConversions
/**
  *
  */
trait RootSyntax {

     implicit class RootLAYEROps[N[_], R:Field](current: N[R])(implicit root: Root[N[R],R]){

          // Root stuff
          def ^(exp: R): N[R] = root.power(current, exp)
          def sqrt(): N[R] = root.squareRoot(current)
          def nRoot(n: R): N[R] = root.nRoot(current, n)
     }
     //Complex(1,2).nRoot(2)

     implicit class RootOps[R:Field](current: R)(implicit root: Root[R,R]) {
          //private val ab: Absolute[R] = implicitly[Absolute[R]]

          // Root stuff
          def ^(exp: R): R = root.power(current, exp)
          def sqrt(): R = root.squareRoot(current)
          def nRoot(n: R): R = root.nRoot(current, n)

          // Absolute stuff
          //def abs(): R = ab.absoluteValue(current)
     }

}

package linalg.syntax

import linalg.numeric.{Root, Root0, Number}
import linalg.theory._

import scala.language.higherKinds
/**
  *
  */
object RootSyntax {

     implicit class RootOps[R: Root: Field](base: R)(implicit root: Root[R]){

          def ^(exp: R): R = root.power(base, exp)
          def sqrt(): R = root.squareRoot(base)
          def nRoot(n: R): R = root.nRoot(base, n)
     }

     implicit class RootBaseOps[H[_], L: Field](base: H[L])(implicit root: Root0[H[L], L],
                                                            numH: Number[H[L]],
                                                            numL: Number[L]){

          def ^(exp: L): H[L] = root.power(base, exp)
          def sqrt(): H[L] = root.squareRoot(base)
          def nRoot(n: L): H[L] = root.nRoot(base, n)
     }
}

package linalg.syntax

import linalg.numeric._
import linalg.theory._

/**
  *
  */
object RootSyntax {
     implicit class RootOps[H, L: Field](base: H)(implicit root: Root0[H, L]){

          def ^(exp: L): H = root.power(base, exp)
          def sqrt(): H = root.squareRoot(base)
          def nRoot(n: L): H = root.nRoot(base, n)
     }
}

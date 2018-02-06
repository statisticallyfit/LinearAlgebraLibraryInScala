package linalg.syntax

import linalg.numeric.Root

/**
  *
  */
object RootSyntax {
     implicit class RootOps[H, L](base: H)(implicit root: Root[H, L]){

          def ^(exp: L): H = root.power(base, exp)
          def sqrt(): H = root.squareRoot(base)
          def nRoot(n: L): H = root.nRoot(base, n)
     }
}

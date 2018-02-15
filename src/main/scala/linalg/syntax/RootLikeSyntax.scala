package linalg.syntax

import linalg.numeric.{RealNumber, RootLike}

/**
  *
  */
trait RootLikeSyntax {

     implicit class RootLikeLAYEROps[N[_], R](current: N[R])(implicit root: RootLike[N[R], R]){

          // Root stuff
          def ^(exp: R): N[R] = root.power(current, exp)
          def sqrt(): N[R] = root.squareRoot(current)
          def nRoot(n: R): N[R] = root.nRoot(current, n)
     }
}

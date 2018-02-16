package linalg.syntax

import linalg._

import scala.language.higherKinds
/**
  *
  */

trait AbsoluteLikeSyntax {

     implicit class AbsoluteLikeLAYEROps[N[_], R](current: N[R])
                                                 (implicit ab: AbsoluteLike[N[R], R]){

          // Absolute stuff
          def abs(): R = ab.absoluteValue(current)
     }
}

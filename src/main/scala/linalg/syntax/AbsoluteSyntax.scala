package linalg.syntax

import linalg.numeric.{Absolute0, Absolute, Number}

import scala.language.implicitConversions
import scala.language.higherKinds

/**
  *
  */
object AbsoluteSyntax {

     implicit class AbsoluteOps[A](current: A)(implicit ab: Absolute[A], num: Number[A]) {
          def abs(): A = ab.absoluteValue(current)
     }

     implicit class AbsoluteBaseOps[H[_], L](current: H[L])(implicit ab: Absolute0[H[L], L],
                                                            numH: Number[H[L]],
                                                            numL: Number[L]){
          def abs(): L = ab.absoluteValue(current)
     }
}

package linalg.syntax

import linalg.theory.Ring

/**
  *
  */

trait RingSyntax extends AbelianGroupSyntax {
     implicit class RingOps[R: Ring](current: R){
          private val ring = implicitly[Ring[R]]

          def *(other: R): R = ring.times(current, other)
     }
}

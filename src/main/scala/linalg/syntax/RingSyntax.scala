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

     //note: for layer types like vector
     /*implicit class RingLayerOps[R[_], N](current: R[N])(implicit ring: Ring[R[N]]){

          def times(other: R[N]): R[N] = ring.times(current, other)
     }*/
}

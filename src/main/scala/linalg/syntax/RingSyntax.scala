package linalg.syntax

import linalg.implicits._
import linalg._

import scala.language.higherKinds
import scala.language.implicitConversions

/**
  *
  */

trait RingSyntax extends AbelianGroupSyntax {
     implicit class RingOps[R: Ring](current: R){
          private val ring = implicitly[Ring[R]]

          def *(other: R): R = ring.times(current, other)
     }

     //note: for layer types like vector but TODO not needed? since no such operation defined
     //for vectors.
     /*implicit class RingLayerOps[R[_], N](current: R[N])(implicit ring: Ring[R[N]]){

          def times(other: R[N]): R[N] = ring.times(current, other)
     }*/
}

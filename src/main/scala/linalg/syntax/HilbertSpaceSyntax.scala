package linalg.syntax

import linalg.implicits._
import linalg._

import scala.language.higherKinds
import scala.language.implicitConversions
/**
  *
  */
trait HilbertSpaceSyntax extends InnerProductSpaceSyntax {

     implicit class HilbertSpaceOps[H[_], F:Field](current: H[F])(implicit hilbert: HilbertSpace[H[F], F]){

          def angle(other: H[F])(implicit t: Trig[F],r: Root[F,F]): F  = hilbert.angle(current, other)
     }
}

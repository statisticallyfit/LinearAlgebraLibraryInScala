package linalg.syntax

import linalg.kernel._
import linalg.theory.Field
import linalg.theory.space.HilbertSpace

import scala.language.higherKinds
/**
  *
  */
trait HilbertSpaceSyntax extends InnerProductSpaceSyntax {

     implicit class HilbertSpaceOps[H[_], F:Field](current: H[F])(implicit hilbert: HilbertSpace[H[F], F]){
          def angle[R:RealNumber](other: H[F])
                                 (implicit t: Trig[F],
                                  r: Root[F,R]): F  = hilbert.angle[R](current, other)
     }
}

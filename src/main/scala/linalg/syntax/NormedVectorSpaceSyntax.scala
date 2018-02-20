package linalg.syntax

import cats.Eq
import linalg.kernel._
import linalg.theory.Field
import linalg.theory.space.NormedVectorSpace

import scala.language.higherKinds
/**
  *
  */
trait NormedVectorSpaceSyntax extends InnerProductSpaceSyntax {

     implicit class NormedVectorSpaceOps[V[_], F:Field](current: V[F])
                                                 (implicit normed: NormedVectorSpace[V[F], F]){


          def norm[R:RealNumber]()(implicit r: Root[F,R]): F = normed.norm[R](current)

          def normalize[R:RealNumber]()(implicit r: Root[F,R]): V[F] = normed.normalize[R](current)

          def isNormalized[R:RealNumber]()(implicit eqVec: Eq[V[F]],  r: Root[F,R]): Boolean =
               normed.isNormalized[R](current)

          def distance[R:RealNumber](other: V[F])(implicit r: Root[F,R]): F =
               normed.distance[R](current, other)
     }
}

package linalg.syntax

import spire.algebra.Eq

import linalg.implicits._
import linalg._

import scala.language.higherKinds
import scala.language.implicitConversions
/**
  *
  */
trait NormedVectorSpaceSyntax extends InnerProductSpaceSyntax {

     implicit class NormedVectorSpaceOps[V[_], F:Field](current: V[F])
                                                 (implicit normed: NormedVectorSpace[V[F], F]){


          def norm()(implicit r: Root[F,F]): F = normed.norm(current)

          def normalize()(implicit r: Root[F,F]): V[F] = normed.normalize(current)

          def isNormalized()(implicit eqVec: Eq[V[F]],  r: Root[F,F]): Boolean =
               normed.isNormalized(current)

          def distance(other: V[F])(implicit r: Root[F,F]): F =
               normed.distance(current, other)
     }
}

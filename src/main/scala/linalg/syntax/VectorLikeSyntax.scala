package linalg.syntax


import linalg._

import scala.language.higherKinds
import scala.language.implicitConversions
/**
  *
  */
trait VectorLikeSyntax extends HilbertSpaceSyntax with NormedVectorSpaceSyntax {

     implicit class VectorLikeOps[V[_], F](current: V[F])(implicit vecLike: VectorLike[V[F], F]/*,
                                                          d: Dimension[V[F]]*/){

          def -(other: V[F]): V[F] = vecLike.minus(current, other)

          def crossProduct(other: V[F]): Option[V[F]] = vecLike.crossProduct(current, other)

          def outerProduct(other: V[F]) = ??? //vecLike.outerProduct(current, other)

          def transpose(): V[F] = vecLike.transpose(current)

          def isZero: Boolean = vecLike.isZero(current)

          def projection(onto: V[F])(implicit f: Field[F], r: Root[F, F]): V[F] = vecLike
               .projection(current, onto)
     }
}

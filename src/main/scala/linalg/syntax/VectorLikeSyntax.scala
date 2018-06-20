package linalg.syntax

import linalg.theory._
import linalg.kernel._
import linalg.theory.basis.Dimension
import linalg.vector._

import scala.language.higherKinds
/**
  *
  */
trait VectorLikeSyntax extends HilbertSpaceSyntax with NormedVectorSpaceSyntax {

     implicit class VectorLikeOps[V[_], F](current: V[F])(implicit vecLike: VectorLike[V[F], F]/*,
                                                          d: Dimension[V[F]]*/){

          def -(other: V[F]): V[F] = vecLike.minus(current, other)

          def crossProduct(other: V[F]): Option[V[F]] = vecLike.crossProduct(current, other)

          def outerProduct(other: V[F]) = ??? //: SetOfVectors[F]= vecLike.outerProduct(current, other)

          def isZero: Boolean = vecLike.isZero(current)

          def projection[R:RealNumber](onto: V[F])(implicit f: Field[F], r: Root[F, R]): V[F] = vecLike
               .projection[R](current, onto)
     }
}

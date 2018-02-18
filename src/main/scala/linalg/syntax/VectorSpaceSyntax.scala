package linalg.syntax

import linalg.theory.space.VectorSpace

import scala.language.higherKinds

/**
  *
  */


trait VectorSpaceSyntax extends AbelianGroupSyntax with MonoidSyntax {

     implicit class VectorSpaceOps[V[_], F](current: V[F])(implicit vecSpace: VectorSpace[V[F], F]){
          def scale(factor: F): V[F] = vecSpace.scale(current, factor)
     }
}

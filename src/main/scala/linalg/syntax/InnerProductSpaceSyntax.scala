package linalg.syntax

import linalg.theory.space.InnerProductSpace

import scala.language.higherKinds
/**
  *
  */
trait InnerProductSpaceSyntax extends VectorSpaceSyntax {

     implicit class InnerProductSpaceOps[I[_], F](current: I[F])(implicit inner: InnerProductSpace[I[F], F]){

          def innerProduct(other: I[F]): F = inner.innerProduct(current, other)
          def dotProduct(other: I[F]): F = inner.dotProduct(current, other)
     }
}

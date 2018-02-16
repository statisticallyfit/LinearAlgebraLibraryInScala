package linalg.syntax

import cats.Eq

//import linalg.kernel.{Abs, Equality, Number, RealNumber, Root, NRoot, Trig}
//import linalg.theory.Field
//import linalg.theory.basis.Dimension
//import linalg.theory.space._
import linalg._
import linalg.vector.{SetOfVectors, Vector}


import scala.language.implicitConversions
import scala.language.higherKinds
/**
  *
  */

//note: finall vec implicits worked in file Vector because I imported linalg.numeric.Number._ in file Vector! (I
// note think that is because Vector file uses Number and so does the implicit class veclikeops)


trait VectorSpaceSyntax {


//     implicit class VectorSpaceOps[V[_], F](current: V[F])(implicit vecSpace: VectorSpace[V[F], F]){
//          def +(other: V[F]): V[F] = vecSpace.plus(current, other)
//          def negate(): V[F] = vecSpace.negate(current)
//          def scale(factor: F): V[F] = vecSpace.scale(current, factor)
//     }

}


trait VectorLikeSyntax extends VectorSpaceSyntax {

//     implicit class VecLikeOps[V[_], N: Number](current: V[N])(implicit vecLike: VectorLike[V[N], N]){
//
//          /** Vector like */
//          def -(other: V[N]): V[N] = vecLike.minus(current, other)
//
//          def angle[R:RealNumber](other: V[N])
//                                 (implicit t: Trig[N],
//                                  r: RootLike[N,R]): N = vecLike.angle[R](current, other)
//
//          def crossProduct(other: V[N]): Option[V[N]] = vecLike.crossProduct(current, other)
//          def outerProduct(other: V[N]): SetOfVectors[N]= vecLike.outerProduct(current, other)
//          def isZero: Boolean = vecLike.isZero(current)
//          def projection[R:RealNumber](onto: V[N])(implicit f: Field[N], r: RootLike[N, R]): V[N] = vecLike
//               .projection[R](current, onto)
//
//          /** Inner product space */
//          def innerProduct(other: V[N]): N = vecLike.innerProduct(current, other)
//          def dotProduct(other: V[N]): N = vecLike.dotProduct(current, other)
//
//          /** Normed vector space */
//          def norm[R:RealNumber]()(implicit r: RootLike[N,R]): N = vecLike.norm[R](current)
//          def normalize[R:RealNumber]()(implicit r: RootLike[N,R]): V[N] = vecLike.normalize[R](current)
//          def isNormalized[R:RealNumber]()(implicit eqVec: Eq[V[N]],  r: RootLike[N,R]): Boolean =
//               vecLike.isNormalized[R](current)
//          def distance[R:RealNumber](other: V[N])(implicit r: RootLike[N,R]): N = vecLike.distance[R](current, other)
//     }
}



trait SetVecLikeSyntax {
//     implicit class SetVecLikeOps[S[_], N: Number](current: S[N])
//                                                  (implicit ev: SetVecLike[S[N], N],
//                                                   dim: Dimension[S[N]]){
//
//          def rowEchelon(): S[N] = ev.rowEchelon(current)
//          def rowReducedEchelon(): S[N] = ev.rowReducedEchelon(current)
//          def dimension(): Int = dim.dimension(current)
//
//          def minus(other: S[N]): S[N] = ev.minus(current, other)
//          def isZero: Boolean = ev.isZero(current)
//     }
}
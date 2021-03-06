package linalg.syntax

import linalg.implicits._
import linalg._

import scala.language.higherKinds
import scala.language.implicitConversions
/**
  *
  */
trait MonoidSyntax {
     implicit class MonoidOps[M: Monoid](current: M){
          private val monoid = implicitly[Monoid[M]]

          def +(other: M): M = monoid.plus(current, other)
     }

     //note: for layer types like vector
     implicit class MonoidLayerOps[M[_], N](current: M[N])(implicit monoid: Monoid[M[N]]){

          def +(other: M[N]): M[N] = monoid.plus(current, other)
     }
}
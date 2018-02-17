package linalg.syntax

import linalg.theory.Monoid

/**
  *
  */
trait MonoidSyntax {
     implicit class MonoidOps[M: Monoid](current: M){
          private val monoid = implicitly[Monoid[M]]

          def +(other: M): M = monoid.plus(current, other)
     }
}
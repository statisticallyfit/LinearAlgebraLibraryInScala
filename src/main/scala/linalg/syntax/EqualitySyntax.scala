package linalg.syntax

import linalg.kernel.Equality
/**
  *
  */
trait EqualitySyntax {

     implicit class EqualityOps[E: Equality](current: E){

          private val comp: Equality[E] = implicitly[Equality[E]]

          //Compare stuff
          //note - must define my own EQ operations :==: and !== because cats.Eq cannot go through Equality and
          // still use === and =!= on Equality types even though it extends Eq ... weird.
          def :==:(other: E): Boolean = comp.eqv(current, other)
          def !==(other: E): Boolean = ! comp.eqv(current, other)
          def <(other: E): Boolean = comp.lessThan(current, other)
          def >(other: E): Boolean = comp.greaterThan(current, other)
          def <=(other: E): Boolean = comp.lessThanOrEqual(current, other)
          def >=(other: E): Boolean = comp.greaterThanOrEqual(current, other)

     }
}
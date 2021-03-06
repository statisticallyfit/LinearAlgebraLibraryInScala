package linalg.matrix

import spire.algebra.Eq

/**
  *
  */
trait LinearSystem[S, F] extends MatrixLike[S, F] {

     //this: Number[N] =>

     //note: linearsystem just contains the actual matrix, not additional right stuff. ???
     def isInconsistent(s: S): Boolean
     def isConsistent(s: S): Boolean  = ! isInconsistent(s)

     def hasNoSolution(s: S): Boolean = isInconsistent(s)
     def hasUniqueSolution(s: S): Boolean
     def hasInfiniteSolutions(s: S): Boolean
     //def hasUniqueSolution(s: S)(implicit e: Eq[S]): Boolean = e.eqv(rowReducedEchelon(s), identity)

     def infiniteSolutionSolver(s: S): Matrix[F]
     def solve(s: S): Option[Matrix[F]]
}

object LinearSystem {
     @inline final def apply[S,F](implicit ev: LinearSystem[S,F]): LinearSystem[S,F] = ev
}

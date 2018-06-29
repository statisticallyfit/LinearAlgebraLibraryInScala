package linalg.syntax

import linalg._
import linalg.matrix.Matrix

import scala.language.higherKinds
import scala.language.implicitConversions
/**
  *
  */
trait LinearSystemSyntax extends MatrixLikeSyntax {

     implicit class LinearSystemOps[S[_], N: Number](current: S[N])(implicit sys: LinearSystem[S[N], N]){

          def isInconsistent(): Boolean = sys.isInconsistent(current)
          def isConsistent(): Boolean = sys.isConsistent(current)
          def hasNoSolution(): Boolean = sys.hasNoSolution(current)
          def hasUniqueSolution(): Boolean = sys.hasUniqueSolution(current)
          def hasInfiniteSolutions(): Boolean = sys.hasInfiniteSolutions(current)
          def infiniteSolutionSolver(): Matrix[N] = sys.infiniteSolutionSolver(current)
          def solve(): Option[Matrix[N]] = sys.solve(current)
     }
}
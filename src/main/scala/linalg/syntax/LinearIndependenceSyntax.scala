package linalg.syntax

import linalg._

import scala.language.higherKinds
import scala.language.implicitConversions
/**
  *
  */
trait LinearIndependenceSyntax {

     implicit class LinearIndependenceOps[V[_], F](current: V[F])
                                                  (implicit indep: LinearIndependence[V[F]]){

          def linearlyIndependent(other: V[F]): Boolean = indep.linearlyIndependent(current, other)
          def isLinearlyIndependent(): Boolean = indep.isLinearlyIndependent(current)
     }
}

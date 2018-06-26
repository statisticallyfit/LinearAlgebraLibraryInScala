package linalg.syntax

import linalg._
import linalg.matrix.Matrix

import scala.language.higherKinds
import scala.language.implicitConversions
/**
  *
  */
trait SpanSyntax {

     implicit class SpanOps[V[_], W[_], N: Number](current: W[N])(implicit ev: Span[V[N], W[N], N]){

          def span(): W[N] = ev.span(current)
          def doesSetSpanTheSpace(): Boolean = ev.doesSetSpanTheSpace(current)
          def doesSetSpanTheVector(vector: V[N]): Boolean = ev.doesSetSpanTheVector(current, vector)
          def isSpanned(vector: V[N]): Boolean = ev.isSpannedBy(vector, current)
          def getSpanningCoefficients(vector: V[N]): Option[W[N]] = ev.getSpanningCoefficients(current, vector)
     }
}
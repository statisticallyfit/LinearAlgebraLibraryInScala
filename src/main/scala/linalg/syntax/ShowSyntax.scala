package linalg.syntax

import linalg._

import scala.language.higherKinds
import scala.language.implicitConversions


/**
  *
  */
trait ShowSyntax {
     implicit class ShowOps[S: Show](current: S) {
          val ev = implicitly[Show[S]]

          def show: String = ev.show(current)
     }
}

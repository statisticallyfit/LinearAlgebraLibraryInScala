package linalg.syntax

import linalg.kernel.Show


/**
  *
  */
trait ShowSyntax {
     implicit class ShowOps[S: Show](current: S) {
          val ev = implicitly[Show[S]]

          def show: String = ev.show(current)
     }
}

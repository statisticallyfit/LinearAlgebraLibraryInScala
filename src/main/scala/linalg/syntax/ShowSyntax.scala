package linalg.syntax

import linalg.util.Show


/**
  *
  */
trait ShowSyntax {
     implicit class ShowOps[S: Show](current: S) {
          val ev = implicitly[Show[S]]

          def show: String = ev.show(current)
     }
}

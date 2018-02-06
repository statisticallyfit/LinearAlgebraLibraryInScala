package linalg.syntax

import linalg.numeric.Absolute

/**
  *
  */
object AbsoluteSyntax {
     implicit class AbsoluteOps[H, L](current: H)(implicit pos: Absolute[H, L]){
          def abs(): L = pos.absoluteValue(current)
     }
}

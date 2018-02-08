package linalg.syntax

import linalg.numeric.Absolute0

/**
  *
  */
object AbsoluteSyntax {
     implicit class AbsoluteOps[H, L](current: H)(implicit pos: Absolute0[H, L]){
          def abs(): L = pos.absoluteValue(current)
     }
}

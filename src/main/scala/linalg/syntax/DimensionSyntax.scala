package linalg.syntax

import linalg._
//import linalg.theory.basis.Dimension
/**
  *
  */
trait DimensionSyntax {

     implicit class DimensionOps[D](current: D)(implicit ev: Dimension[D]){
          def dimension(): Int = ev.dimension(current)
     }
}

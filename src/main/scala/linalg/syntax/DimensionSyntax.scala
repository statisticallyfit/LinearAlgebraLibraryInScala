package linalg.syntax


import linalg.theory.basis.Dimension
/**
  *
  */
object DimensionSyntax {

     implicit class DimensionOps[D](current: D)(implicit ev: Dimension[D]){
          def dimension(): Int = ev.dimension(current)
     }
}

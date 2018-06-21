package linalg.syntax

import linalg.implicits._
import linalg._

import scala.language.higherKinds
import scala.language.implicitConversions
/**
  *
  */
trait DimensionSyntax {

     implicit class DimensionOps[D](current: D)(implicit ev: Dimension[D]){
          def dimension(): Int = ev.dimension(current)
     }
}

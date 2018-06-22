package linalg.matrix

import linalg._
import linalg.implicits._
import linalg.util._

/**
  *
  */
class OrthogonalMatrix[N: Number](smat: SquareMatrix[N]) extends SquareMatrix[N](smat) {
     require(Util.MatrixOps.Id.isOrthogonal(smat))
}

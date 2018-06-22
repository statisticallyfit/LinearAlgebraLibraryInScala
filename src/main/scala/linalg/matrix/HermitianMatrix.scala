package linalg.matrix

import linalg._
import linalg.implicits._
import linalg.util._

/**
  *
  */
class HermitianMatrix[N: Number](smat: SquareMatrix[N]) extends SquareMatrix[N](smat) {
     require(Util.MatrixOps.Id.isHermitian(smat))
}

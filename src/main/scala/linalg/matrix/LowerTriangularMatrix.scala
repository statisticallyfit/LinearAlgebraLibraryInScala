package linalg.matrix


import linalg._
import linalg.implicits._
import linalg.util._
/**
  *
  */
class LowerTriangularMatrix[N: Number](mat: Matrix[N]) extends Matrix[N](mat.getColumns():_*) {
     require(Util.MatrixOps.Id.isLowerTriangular(mat))
}

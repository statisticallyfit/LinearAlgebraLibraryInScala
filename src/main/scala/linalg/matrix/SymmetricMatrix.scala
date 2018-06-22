package linalg.matrix


import linalg._
import linalg.implicits._
import linalg.util._
import linalg.vector.Vector

/**
  *
  */
class SymmetricMatrix[N: Number](smat: SquareMatrix[N]) extends SquareMatrix[N](smat) {
     require(Util.MatrixOps.Id.isSymmetric(smat))
}
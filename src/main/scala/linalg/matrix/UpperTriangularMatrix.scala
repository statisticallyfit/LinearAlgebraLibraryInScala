package linalg.matrix

import linalg.util._
import linalg._
import linalg.implicits._
import linalg.vector.SetOfVectors

/**
  *
  */
class UpperTriangularMatrix[N: Number](mat: Matrix[N]) extends Matrix[N](mat.getColumns():_*) {
     require(Util.MatrixOps.Id.isUpperTriangular(mat))

     //def this(mat: SetOfVectors[N]) = this(new Matrix(mat)) //TODO what is the goal here?
}
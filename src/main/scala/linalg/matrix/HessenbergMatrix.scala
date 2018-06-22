package linalg.matrix


import linalg._
import linalg.implicits._
import linalg.util._
/**
  *
  */
class HessenbergMatrix[N : Number](smat: SquareMatrix[N])
     extends SquareMatrix[N](Util.MatrixOps.Trans.HessenbergTransformer.makeHessenberg(smat))

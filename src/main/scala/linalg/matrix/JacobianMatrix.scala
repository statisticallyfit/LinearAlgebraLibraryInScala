package linalg.matrix

import linalg._
import linalg.implicits._
import linalg.util._
import linalg.vector.Vector

/**
  *
  */
class JacobianMatrix[N: Number](cols: Vector[N]*) extends Matrix[N](cols:_*)
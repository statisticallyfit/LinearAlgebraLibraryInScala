package linalg.matrix


import linalg._
import linalg.implicits._
import linalg.util._
import linalg.vector.Vector

/**
  *
  */
class DiagonalMatrix[N: Number](diagonal: Vector[N]) extends Matrix[N](diagonal)

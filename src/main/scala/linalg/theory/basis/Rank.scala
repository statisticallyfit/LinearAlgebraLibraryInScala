package linalg.theory.basis


import linalg.numeric._
import linalg.theory.space._
import linalg.theory._

/**
  *
  *
  */

trait Rank[V, F] extends VectorSpace[V, F] {

     def rank(vectorSpace: V): Int
}
package linalg.instances

import linalg.kernel._
import linalg.instances.linear._
/**
  *
  */
trait VectorLikeInstances {
     implicit def vectorIsLikeAVector[N: Number] = new VectorInstances[N].vectorLike
}

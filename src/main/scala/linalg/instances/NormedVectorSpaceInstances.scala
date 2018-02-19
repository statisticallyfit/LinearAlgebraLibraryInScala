package linalg.instances

import linalg.kernel._
import linalg.instances.linear._
/**
  *
  */
trait NormedVectorSpaceInstances {
     implicit def vectorIsNormedVectorSpace[N: Number] = new VectorInstances[N].normedSpace
}

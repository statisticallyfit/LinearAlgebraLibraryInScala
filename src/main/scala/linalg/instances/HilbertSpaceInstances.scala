package linalg.instances

import linalg.kernel._
import linalg.instances.linear._
/**
  *
  */
trait HilbertSpaceInstances {

     implicit def vectorIsHilbertSpace[N: Number] = new VectorInstances[N].hilbertSpace
}

package linalg.instances

import linalg.kernel._
import linalg.instances.linear._
/**
  *
  */
trait HilbertSpaceInstances {

     implicit def vectorIsHilbertSpace[N: Number] = new VectorThings[N].hilbertSpace
}

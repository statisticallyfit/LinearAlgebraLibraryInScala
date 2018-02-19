package linalg.instances

import linalg.kernel._
import linalg.instances.linear._
/**
  *
  */
trait InnerProductSpaceInstances {

     implicit def vectorIsInnerProductSpace[N: Number] = new VectorInstances[N].innerSpace
}

package linalg.instances

import linalg.kernel._
import linalg.instances.linear._
/**
  *
  */
trait AbelianGroupInstances {
     implicit def vectorIsAbelianGroup[N: Number] = new VectorInstances[N].abelian
}

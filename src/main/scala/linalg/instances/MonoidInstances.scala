package linalg.instances

import linalg.kernel._
import linalg.instances.linear._
/**
  *
  */
trait MonoidInstances {

     implicit def vectorIsMonoid[N: Number] = new VectorInstances[N].monoid
}

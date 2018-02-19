package linalg.instances

import linalg.instances.linear.VectorInstances
import linalg.kernel._

/**
  *
  */
trait DimensionInstances {


     implicit def vectorHasDimension[N: Number] = new VectorInstances[N].dim
}

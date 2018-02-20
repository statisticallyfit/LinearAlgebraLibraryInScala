package linalg.instances

import linalg.instances.linear.{SetVecThings, VectorThings}
import linalg.kernel._

/**
  *
  */
trait DimensionInstances {


     implicit def vectorHasDimension[N: Number] = new VectorThings[N].dim
     implicit def setVecHasDimension[N: Number] = new SetVecThings[N].dim
}

package linalg.instances

import linalg.instances.linear.{SetVecInstances, VectorInstances}
import linalg.kernel._

/**
  *
  */
trait DimensionInstances {


     implicit def vectorHasDimension[N: Number] = new VectorInstances[N].dim
     implicit def setVecHasDimension[N: Number] = new SetVecInstances[N].dim
}

package linalg.instances

//import linalg.instances.linear._
import linalg.implicits._
import linalg._
import linalg.instances.linear.{SetVecThings, VectorThings}

/**
  *
  */
trait DimensionInstances {


     implicit def vectorHasDimension[N: Number] = new VectorThings[N].dim
     implicit def setVecHasDimension[N: Number] = new SetVecThings[N].dim
}

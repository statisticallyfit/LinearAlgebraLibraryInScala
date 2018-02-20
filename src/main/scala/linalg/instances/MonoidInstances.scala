package linalg.instances

import linalg.kernel._
import linalg.instances.linear._
/**
  *
  */
trait MonoidInstances {

     implicit def vectorIsMonoid[N: Number] = new VectorInstances[N].monoid
     implicit def setVecIsMonoid[N: Number] = new SetVecInstances[N].monoid
}

package linalg.instances

import linalg.instances.linear.SetVecInstances
import linalg.kernel.Number

/**
  *
  */
trait SetVecLikeInstances {


     implicit def setVecIsSetVecLike[N: Number] = new SetVecInstances[N].vsetLike
}

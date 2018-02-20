package linalg.instances

import linalg.instances.linear.SetVecThings
import linalg.kernel.Number

/**
  *
  */
trait SetVecLikeInstances {


     implicit def setVecIsSetVecLike[N: Number] = new SetVecThings[N].vsetLike
}

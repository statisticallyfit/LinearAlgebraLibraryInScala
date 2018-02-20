package linalg.instances

import linalg.implicits._
import linalg.instances.linear._
import linalg.kernel._
import linalg.vector._
import linalg.theory.space._
import linalg.theory._
import linalg.util.Util
/**
  *
  */



trait VectorSpaceInstances {


     implicit def vectorIsVectorSpace[N: Number] = new VectorInstances[N].vectorSpace
     implicit def setVecIsVectorSpace[N: Number] = new SetVecInstances[N].vectorSpace
}

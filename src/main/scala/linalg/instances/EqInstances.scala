package linalg.instances

import cats.Eq
import cats.implicits._

import linalg._
import linalg.util.Util
import linalg.vector.{SetOfVectors, Vector}
/**
  *
  */
trait EqInstances {

//     implicit def eqVector[N: Number] = new Eq[Vector[N]]{
//          def eqv(v: Vector[N], w: Vector[N]): Boolean = v.getElements() == w.getElements()
//     }
//
//     implicit def eqSetOfVectors[N: Number] = new Eq[SetOfVectors[N]]{
//          def eqv(vset: SetOfVectors[N], wset: SetOfVectors[N]): Boolean = {
//               Util.Gen.ensureSize(vset, wset)
//               vset.getColumns().zip(wset.getColumns()).forall(colPair => colPair._1 === colPair._2)
//          }
//     }
}

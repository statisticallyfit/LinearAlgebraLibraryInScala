package linalg.instances

import linalg.implicits._
import linalg._
import linalg.vector.{SetOfVectors, Vector}
import linalg.util._

import spire.algebra.Eq

/**
  *
  */
trait EqInstances {

     /*implicit def eqVector[N: Number] = new Eq[Vector[N]]{

          def eqv(v: Vector[N], w: Vector[N]): Boolean = Eq[Seq[N]].eqv(v.getElements(), w.getElements())
     }

     implicit def eqSetOfVectors[N: Number] = new Eq[SetOfVectors[N]]{

          def eqv(vset: SetOfVectors[N], wset: SetOfVectors[N]): Boolean = {
               Util.Gen.ensureSize(vset, wset)

               vset.getColumns()
                    .zip(wset.getColumns())
                    .forall(colPair => Eq[Vector[N]].eqv(colPair._1, colPair._2))
          }
     }*/
}

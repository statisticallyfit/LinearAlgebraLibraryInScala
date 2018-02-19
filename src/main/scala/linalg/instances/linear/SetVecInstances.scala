package linalg.instances.linear

import linalg.implicits._
import linalg.kernel.{Number, RealNumber, Root, Trig}
import linalg.theory.basis.Dimension
import linalg.theory.{AbelianGroup, Field, Monoid}
import linalg.theory.space.{HilbertSpace, InnerProductSpace, NormedVectorSpace, VectorSpace}
import linalg.util.Util
import linalg.vector.{SetOfVectors, Vector, VectorLike}

import scala.collection.mutable.Seq
/**
  *
  */
class SetVecInstances[N: Number] {

     class SetVecIsMonoid extends Monoid[SetOfVectors[N]]{

          val zero: SetOfVectors[N] = SetOfVectors(Vector.ZERO[N](1))

          def plus(vset: SetOfVectors[N], wset: SetOfVectors[N]): SetOfVectors[N] ={
               Util.Gen.ensureSize(vset, wset)
               SetOfVectors(vset.getColumns().zip(wset.getColumns())
                    .map(colPair => colPair._1 + colPair._2):_*)
          }
     }

     class SetVecIsAbelianGroup extends SetVecIsMonoid with AbelianGroup[SetOfVectors[N]]{
          def negate(vset: SetOfVectors[N]): SetOfVectors[N] = SetOfVectors(vset.getColumns().map(c => c.negate()):_*)
     }

     class SetVecIsVectorSpace extends SetVecIsAbelianGroup with VectorSpace[SetOfVectors[N], N]{

     }
}

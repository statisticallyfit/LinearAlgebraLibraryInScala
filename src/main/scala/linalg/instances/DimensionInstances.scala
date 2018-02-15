package linalg.instances

import linalg.implicits._

import linalg.kernel.Number
import linalg.theory.basis.Dimension
import linalg.vector.{SetOfVectors, Vector, VectorLike}

/**
  *
  */
trait DimensionInstances {

     implicit def vectorHasDimension[N: Number] = new Dimension[Vector[N]] {
          def dimension(v: Vector[N]): Int = v.getElements().length
     }

     implicit def setOfVectorsHasDimension[N: Number] = new Dimension[SetOfVectors[N]]{
          def dimension(vset: SetOfVectors[N]): Int = vset.getColumns().head.dimension()
     }
}

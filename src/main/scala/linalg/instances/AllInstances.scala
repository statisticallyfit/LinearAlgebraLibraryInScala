package linalg.instances

import linalg.instances.linear._
import linalg.instances.std._


/**
  *
  */
//note need to have intinstances etc
//note: or maybe: RealNumberInstaces extends IntIsRealNumber with DoubleisReal, leaving IntInstances aside
// and so forth: DimensionInstances extends VectorHasDimension, leaving VectorInstances aside
trait AllInstances
     extends NumberInstances
     with RealNumberInstances
     with EqInstances
     with EqualityInstances
     with ShowInstances
     with MonoidInstances
     with AbelianGroupInstances
     with InnerProductSpaceInstances
     with NormedVectorSpaceInstances
     with HilbertSpaceInstances
     with VectorSpaceInstances
     with VectorLikeInstances
     with SetVecLikeInstances
     with DimensionInstances
     with VectorInstances
     with SetVecInstances


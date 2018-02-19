package linalg.instances

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
     with ShowInstances
     with MonoidInstances
     with AbelianGroupInstances
     with InnerProductSpaceInstances
     with NormedVectorSpaceInstances
     with VectorSpaceInstances
     with VectorLikeInstances
     with SetVecLikeInstances
     with DimensionInstances
     with EqInstances
     with EqualityInstances


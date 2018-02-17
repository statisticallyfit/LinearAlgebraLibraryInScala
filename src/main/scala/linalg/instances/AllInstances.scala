package linalg.instances

import linalg.instances.old._
import linalg.instances.std._


/**
  *
  */
//note need to have intinstances etc
//note: or maybe: RealNumberInstaces extends IntIsRealNumber with DoubleisReal, leaving IntInstances aside
// and so forth: DimensionInstances extends VectorHasDimension, leaving VectorInstances aside
trait AllInstances
     extends IntInstances
     with DoubleInstances
     with RealInstances
     with RationalInstances
     with ComplexInstances

/*NumberInstances
     with NumericConversionInstances
     with VectorSpaceInstances
     with DimensionInstances
     with EqInstances
     with EqualityInstances
     with ShowInstances*/

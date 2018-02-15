package linalg.theory


import linalg.theory.space._

/**
  *
  */

trait LinearIndependence[V, F] {

     def areLinearlyIndependent(v: V, w: V): Boolean //compares two vecsets or single vecs ...

     def isLinearlyIndependent(v: V): Boolean //can be passed a single vec or vecset
}

object LinearIndependence {
     @inline final def apply[V,F](implicit ev: LinearIndependence[V,F]): LinearIndependence[V,F] = ev
}
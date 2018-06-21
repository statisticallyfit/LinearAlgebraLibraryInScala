package linalg.theory.basis

/**
  *
  */

trait LinearIndependence[V] {

     def linearlyIndependent(v: V, w: V): Boolean //compares two vecsets or single vecs ...

     def isLinearlyIndependent(v: V): Boolean //can be passed a single vec or vecset
}

object LinearIndependence {
     @inline final def apply[V](implicit ev: LinearIndependence[V]): LinearIndependence[V] = ev
}
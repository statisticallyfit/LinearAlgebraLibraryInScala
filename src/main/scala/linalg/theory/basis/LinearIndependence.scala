package linalg.theory


import linalg.theory.space._

/**
  *
  */

trait LinearIndependence[V, F] {

     def linearlyIndependent(v: V, w: V): Boolean //compares two vecsets or single vecs ...

     def linearlyIndependent(v: V): Boolean //can be passed a single vec or vecset
}
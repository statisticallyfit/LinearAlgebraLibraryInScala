package linalg.theory.basis

/**
  *
  * pg 342 for linear transformation LAWS (addition and scalar multiplication)
  *
  */
trait LinearTransformation[V, W] {

     def transform(x: V): W
}



object LinearTransformation {
     final def apply[V, W](implicit ev: LinearTransformation[V, W]): LinearTransformation[V, W] = ev
}
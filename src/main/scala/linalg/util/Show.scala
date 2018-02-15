package linalg.util

/**
  *
  */
//todo use cats show?

trait Show[S] {

     def show(x: S): String
}
object Show {

     @inline final def apply[S](implicit ev: Show[S]): Show[S] = ev
}
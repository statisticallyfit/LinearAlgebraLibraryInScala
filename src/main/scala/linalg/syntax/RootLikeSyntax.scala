package linalg.syntax

//import linalg.kernel.{RealNumber, NRoot}
import linalg._
import linalg.implicits._
import linalg.kernel.Complex


import scala.language.higherKinds
/**
  *
  */
trait RootLikeSyntax {

     implicit class RootLikeLAYEROps[N[_], R:RealNumber](current: N[R])(implicit root: RootLike[N[R],R]){

          // Root stuff
          def ^(exp: R): N[R] = root.power(current, exp)
          def sqrt(): N[R] = root.squareRoot(current)
          def nRoot(n: R): N[R] = root.nRoot(current, n)
     }
     Complex(1,2).nRoot(2)

     /*implicit class RootOps[R:Root](current: R){
          private val root: Root[R] = implicitly[Root[R]]
          //private val ab: Absolute[R] = implicitly[Absolute[R]]

          // Root stuff
          def ^(exp: R): R = root.power(current, exp)
          def sqrt(): R = root.squareRoot(current)
          def nRoot(n: R): R = root.nRoot(current, n)

          // Absolute stuff
          //def abs(): R = ab.absoluteValue(current)
     }*/

}

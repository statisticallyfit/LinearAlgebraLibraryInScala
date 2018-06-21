package linalg.syntax

import linalg.implicits._
import linalg._

import scala.language.higherKinds
import scala.language.implicitConversions
/**
  *
  */

trait AbsoluteSyntax {

     implicit class AbsoluteLayerOps[N[_], R](current: N[R])(implicit ab: Absolute[N[R], R]){
          def abs(): R = ab.absoluteValue(current)
     }

     implicit class AbsoluteOps[R](current: R)(implicit ab: Absolute[R, R]){
          def abs(): R = ab.absoluteValue(current)
     }
}

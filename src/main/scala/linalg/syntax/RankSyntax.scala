package linalg.syntax

import linalg.implicits._
import linalg._

import scala.language.higherKinds
import scala.language.implicitConversions
/**
  *
  */
trait RankSyntax {

     implicit class RankOps[V[_], N: Number](current: V[N])(implicit ev: Rank[V[N]]){

          def rank(): Int = ev.rank(current)
          def isFullRank(): Boolean = ev.isFullRank(current)
     }
}

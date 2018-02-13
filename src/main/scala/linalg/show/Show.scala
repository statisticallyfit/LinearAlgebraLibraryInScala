package linalg.show

import linalg.numeric._
import linalg.vector._
import org.apache.commons.lang3.StringUtils

import scala.collection.mutable.Seq

/**
  *
  */

trait Show[S] {

     def show(x: S): String
}
object Show {

     implicit object IntHasShow extends Show[Int] {def show(x: Int): String = x.toString}
     implicit object DoubleHasShow extends Show[Double] {def show(x: Double): String = x.toString}
     implicit object RealHasShow extends Show[Real] { def show(x: Real): String = x.double.toString }

     implicit object RationalHasShow extends Show[Rational] {
          def show(x: Rational): String = x.den match {
               case 1 => x.num.toString
               case _ => x.num.toString + "/" + x.den.toString
          }
     }

     implicit def ComplexHasShow[R : RealNumber] = new Show[Complex[R]] {
          def show(x: Complex[R]): String = x.re.toString + Imaginary(x.im).toString
     }

     implicit def VectorHasShow[N: Number] = new Show[Vector[N]]{
          def show(v: Vector[N]): String = showVecSet(SetOfVectors(v))
     }

     //todo
     /*implicit def PolynomialHasShow[R: RealNumber] = new Show[Polynomial[R]]{
          def show(poly: Polynomial[R]): String = showVecSet(SetOfVectors(poly))
     }*/

     implicit def SetVecHasShow[N: Number] = new Show[SetOfVectors[N]]{
          def show(vset: SetOfVectors[N]): String = showVecSet(vset)
     }

     private def showVecSet[N:Number](vset: SetOfVectors[N]): String ={
          val colsStr: Seq[Seq[String]] = vset.getColumns().map(vec => vec.getElements().map(elem => elem.toString))

          // max widths measured per col
          val maxWidths: Seq[Int] = colsStr.map(vec => vec.reduceLeft((acc,y) =>
               if(acc.length > y.length) acc else y)).map(_.length)

          val maxWidthsTwoDim: Seq[Seq[Int]] = maxWidths.map(elem => Seq.fill(vset.numRows)(elem))

          // col center length tupled with actual matrix col element in vector of vectors
          val pairs: Seq[Seq[(String, Int)]] = colsStr.zip(maxWidthsTwoDim).map(pair => pair._1.zip(pair._2))
          val aligned: Seq[Seq[String]] = pairs.map(vec => vec.map(pair => StringUtils.leftPad(pair._1.toString, pair._2)))

          // note: let maxWidth + 2 separate the numbers in the row
          if(vset.numRows == 1)
               return "\n{" + aligned.transpose.head.mkString("  ") + "}"
          val firstRow: String = "\n/ " + aligned.transpose.head.mkString("  ") + " \\\n"
          val lastRow: String = "\\ " + aligned.transpose.last.mkString("  ") + " /"
          val middleRows: Seq[String] = aligned.transpose.tail.init
               .map(list => "| " + list.mkString("  ") + " |\n")

          firstRow + middleRows.mkString + lastRow
     }
}
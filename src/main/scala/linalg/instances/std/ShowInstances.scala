package linalg.instances.std

import linalg.implicits._
import linalg._
import linalg.kernel.{Complex, Imaginary, Rational, Real}
import linalg.matrix.{AugmentedMatrix, Matrix}
import linalg.vector.{Polynomial, SetOfVectors, Vector}
import org.apache.commons.lang3.StringUtils
import linalg.util._

import scala.collection.mutable.ListBuffer
import scala.language.higherKinds
import scala.language.implicitConversions


/**
  *
  */
trait ShowInstances {

     implicit object IntHasShow extends Show[Int] {def show(x: Int): String = x.toString}

     implicit object DoubleHasShow extends Show[Double] {
          def show(x: Double): String = {
               if(x == 0) {
                    "0"
               } else {
                    val (first, last): (String, String) = x.toString.splitAt(x.toString.indexOf("."))

                    if(last.toDouble == 0) first else x.toString
               }
          }
     }

     implicit object RealHasShow extends Show[Real] { def show(x: Real): String = x.double.show }


     implicit object RationalHasShow extends Show[Rational] {
          def show(x: Rational): String = x.den match {
               case 1 => x.num.toString
               case _ => x.num.toString + "/" + x.den.toString
          }
     }

     implicit def ComplexHasShow[R : RealNumber] = new Show[Complex[R]] {
          def show(x: Complex[R]): String = x.re.toString + Imaginary(x.im).toString
     }

     implicit def ImaginaryHasShow[R:RealNumber] = new Show[Imaginary[R]]{
          def show(x: Imaginary[R]): String = x.im match {
               case _: Rational => x.im.isNegative match {
                    case true => " - (" + x.im.negate().toString + ")" + "i"
                    case false => " + (" + x.im.toString + ")" + "i"
               }
               case _ => x.im.isNegative match {
                    case true => " - " + x.im.negate().toString + "i"
                    case false => " + " + x.im.toString + "i"
               }
          }
     }

     implicit def VectorHasShow[N: Number] = new Show[Vector[N]]{

          def show(v: Vector[N]): String = {

               v.isCol() match {
                    case true => showVecSet(SetOfVectors(v))
                    case false => showVecSet(Util.transpose(SetOfVectors(v))) //torowvec
               }
          }
     }

     //todo
     implicit def PolynomialHasShow[R: RealNumber] = new Show[Polynomial[R]]{
          def show(poly: Polynomial[R]): String = showPoly(poly)
     }

     private def showPoly[R: RealNumber](poly: Polynomial[R]): String = ???

     implicit def SetVecHasShow[N: Number] = new Show[SetOfVectors[N]]{
          def show(vset: SetOfVectors[N]): String = showVecSet(vset)
     }

     implicit def AugmentedMatrixHasShow[N: Number] = new Show[AugmentedMatrix[N]] {
          def show(mat: AugmentedMatrix[N]): String = showAugMatrix(mat)

     }


     private def showAugMatrix[N:Number](mat: AugmentedMatrix[N]): String ={
          val colsStr: Seq[Seq[String]] = mat.getColumns().map(vec => vec.getElements().map(e => e.toString))

          // max widths measured per col
          val maxWidths: Seq[Int] = colsStr.map(vec => vec.reduceLeft((acc,y) =>
               if(acc.length > y.length) acc else y)).map(_.length)

          val maxWidthsTwoDim: Seq[Seq[Int]] = maxWidths.map(elem => Seq.fill(mat.numRows)(elem))

          // col center length tupled with actual matrix col element in vector of vectors
          val pairs: Seq[Seq[(String, Int)]] = colsStr.zip(maxWidthsTwoDim).map(pair => pair._1.zip(pair._2))
          val alignedCols: Seq[Seq[String]] = pairs.map(vec => vec.map(pair => StringUtils.leftPad(pair._1.toString, pair._2)))

          var sepAlignedCols: Seq[Seq[String]] = alignedCols.take(mat.A.numCols) :+ Seq.fill[String](mat.A.numRows)("|")
          sepAlignedCols = sepAlignedCols ++ alignedCols.drop(mat.A.numCols)

          // note: let maxWidth + 2 separate the numbers in the row
          if(mat.numRows == 1)
               return "\n{" + sepAlignedCols.transpose.head.mkString("  ") + "}"
          val firstRow: String = "\n/ " + sepAlignedCols.transpose.head.mkString("  ") + " \\\n"
          val lastRow: String = "\\ " + sepAlignedCols.transpose.last.mkString("  ") + " /"
          val middleRows: Seq[String] = sepAlignedCols.transpose.tail.init.map(list => "| " + list.mkString("  ") + " |\n")

          firstRow + middleRows.mkString + lastRow
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

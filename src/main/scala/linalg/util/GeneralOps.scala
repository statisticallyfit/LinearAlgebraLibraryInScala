package linalg.util

import linalg._
import linalg.implicits._

/**
  *
  */
trait GeneralOps {

     /**
       * Inserts element at position i, leaving list the same length as before.
       */
     def insert[N: Number](elem: N, i: Int, list: Seq[N]): Seq[N] = {
          val (first, second) = list.splitAt(i)
          (first :+ elem) ++: second.tail
     }

     /**
       * Inserts list at position i, leaving list of lists the same length as before.
       */
     def insert[N: Number](elems: Seq[N], i: Int, list: Seq[Seq[N]]): Seq[Seq[N]] = {
          val (first, second) = list.splitAt(i)
          (first :+ elems) ++: second.tail
     }

     def roundTo(double: Double, places: Int): Double = {
          //step 1: multiply by the correct number of zeroes = numplaces
          val factor: Double = ("1" + List.fill[Int](places)(0).mkString).toDouble
          (double * factor).round / factor
     }

}
package linalg.util

import linalg._
import linalg.vector.Vector
import linalg.implicits._

import scala.collection.mutable
import scala.collection.mutable.Seq

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
       * Inserts list at position i , leaving list of lists the same length as before.
       */
     def insertVec[N: Number](vec: Vector[N], i: Int, list: Seq[Vector[N]]): Seq[Vector[N]] = {
          val (firstVecs, secondVecs) = list.splitAt(i)
          (firstVecs :+ vec) ++: secondVecs.tail
     }

     def roundTo(double: Double, places: Int): Double = {
          //step 1: multiply by the correct number of zeroes = numplaces
          val factor: Double = ("1" + List.fill[Int](places)(0).mkString).toDouble
          (double * factor).round / factor
     }

}
package linalg.kernel

/**
  *
  */

trait NumericConversion[A, B, C] {

     def plus(from: A, to: B): C
     def minus(from: A, to: B): C
     def times(from: A, to: B): C
     def divide(from: A, to: B): C
}


object NumericConversion

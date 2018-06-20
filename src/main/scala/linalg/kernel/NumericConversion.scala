package linalg.kernel

/**
  *
  */
trait NumericConversion[F, T] {
     def plus(from: F, to: T): T
     def minus(from: F, to: T): T
     def times(from: F, to: T): T
     def divide(from: F, to: T): T
     //def exponentiate(base: T, exp: F): T
}

trait NumericConversion2[T, F] {
     def plus(upper: T, from:F): T
     def minus(upper: T, from: F): T
     def times(upper: T, from: F): T
     def divide(upper: T, from: F): T
     //def exponentiate(base: T, exp: F): T

}

object NumericConversion

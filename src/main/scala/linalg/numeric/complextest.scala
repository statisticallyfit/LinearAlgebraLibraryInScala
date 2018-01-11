//package linalg.numeric
//
//
//import spire.math.{Complex, Rational, Real}
//import scala.language.implicitConversions
//
//
//
//
//object complextest extends App {
//
//     //note: YAY! not even spire has interoperability between types! Then I don't have to do it!
//     //note YAY! - spire's doesn't work either - so mine shouldn't then.
//     //note: the best I can do, like them, is to do interoperability between
//     //complex and the type it holds.
//     println(Complex(1,2) + Real(1) + Rational(2))
//     println(Complex(1,2) + 1)
//     println(1 + Complex(1,2))
//
//     /*trait Threader[T] {
//          def plus(x: T, y: T): T
//     }
//     implicit class ThreaderOps[T](current: T)(implicit threader: Threader[T]) {
//
//          def +(other: T): T = threader.plus(current, other)
//     }
//     implicit object RealIsThreaded extends Threader[RealClass]{
//          def plus(x: RealClass, y: RealClass): RealClass = RealClass(x.double + y.double)
//     }
//     implicit object NatIsThreaded extends Threader[NaturalClass]{
//          def plus(x: NaturalClass, y: NaturalClass): NaturalClass = NaturalClass(x.int + y.int)
//     }
//
//     object InterOps {
//          implicit class RealInterOps[T](real: RealClass)(implicit t: Threader[T]){
//               def +(other: T): RealClass = other match {
//                    case _: NaturalClass => NaturalClass()
//               }
//          }
//     }
//
//
//     case class RealClass(double: Double)
//
//     case class NaturalClass(int: Int)
//
//     println(RealClass(2) + NaturalClass(3))*/
//
//     // can't do this with spire unfortunately, so must build my own if wanting to be fancy.
//     //val c: Complex[Int] = 3 + 3*j
//}
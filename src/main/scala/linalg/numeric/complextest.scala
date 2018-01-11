package linalg.numeric


//import spire.math.{Complex, Rational}
import scala.language.implicitConversions




object complextest extends App {

     trait Threader[T] {
          def plus(x: T, y: T): T
     }
     implicit class ThreaderOps[T](current: T)(implicit threader: Threader[T]) {

          def +(other: T): T = threader.plus(current, other)
     }
     implicit object RealIsThreaded extends Threader[RealClass]{
          def plus(x: RealClass, y: RealClass): RealClass = RealClass(x.double + y.double)
     }
     implicit object NatIsThreaded extends Threader[NaturalClass]{
          def plus(x: NaturalClass, y: NaturalClass): NaturalClass = NaturalClass(x.int + y.int)
     }

     object InterOps {
          implicit class RealInterOps[T](real: RealClass)(implicit t: Threader[T]){
               def +(other: T): RealClass = other match {
                    case _: NaturalClass => NaturalClass()
               }
          }
     }


     case class RealClass(double: Double)

     case class NaturalClass(int: Int)

     println(RealClass(2) + NaturalClass(3))

     // can't do this with spire unfortunately, so must build my own if wanting to be fancy.
     //val c: Complex[Int] = 3 + 3*j
}
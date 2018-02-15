package linalg.kernel

import linalg.theory.Field


trait NRoot[N, R] extends Field[R] { //is field

     //val rOne: R
     //val rTwo: R
     val two: R = plus(one, one)

     //implicit def num: Number[N]

     def power(base: N, exp: R): N
     def nRoot(base: N, n: R)(implicit f: Field[R]): N = power(base, f.divide(one, n))
     def squareRoot(base: N)(implicit f: Field[R]): N = nRoot(base, two)
}

object NRoot {
     @inline final def apply[N,R](implicit ev: NRoot[N,R]): NRoot[N,R] = ev
}


trait Root[R] extends NRoot[R, R]

object Root {
     @inline final def apply[R](implicit ev: Root[R]): Root[R] = ev
}
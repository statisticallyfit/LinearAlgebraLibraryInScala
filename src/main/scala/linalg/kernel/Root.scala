package linalg.kernel

import linalg.theory.Field


//1 todo what happens when we just have implicits like def two[implicit field}
//or 2 todo what happens when implicit def field: Field[R] and we use field.divide(field.plus(...))??
// 3 todo contrast with FIeld[R] and if implicit error ambigious is given?


//todo look at spire's Root to see how they have implicit number: Number[N] there!

trait Root[N, R] {
     //def two(implicit f: Field[R]): R = f.plus(f.one, f.one)

     def power(base: N, exp: R): N
     def nRoot(base: N, n: R)(implicit f: Field[R]): N = power(base, f.divide(f.one, n))
     def squareRoot(base: N)(implicit f: Field[R]): N = nRoot(base, f.plus(f.one, f.one))
}

object Root {
     @inline final def apply[N,R](implicit ev: Root[N,R]): Root[N,R] = ev
}

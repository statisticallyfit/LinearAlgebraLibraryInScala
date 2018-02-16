package linalg.kernel




//1 todo what happens when we just have implicits like def two[implicit field}
//or 2 todo what happens when implicit def field: Field[R] and we use field.divide(field.plus(...))??
// 3 todo contrast with FIeld[R] and if implicit error ambigious is given?
trait RootLike[N, R] extends /*linalg.Field[R]*/ { //is field

     //val rOne: R
     //val rTwo: R
     //private def two(implicit r: RealNumber[R]): R = r.plus(r.one, r.one)

     //implicit def num: Number[N]

     //note: technically we are allowed here NRoot[Number,Number] but must hvae
     //Number implicit not RealNumber implicit because we want to use NRoot[Vec[N] ,N]
     //so minimally N must be a number, no more specific than that.
     //type R <: RealNumber[R]

     def power(base: N, exp: R): N
     def nRoot(base: N, n: R)(implicit r: Number[R]): N = power(base, r.divide(r.one, n))
     def squareRoot(base: N)(implicit r: Number[R]): N = nRoot(base, r.two)
}

object RootLike {
     @inline final def apply[N,R](implicit ev: RootLike[N,R]): RootLike[N,R] = ev
}


trait Root[R] extends RootLike[R, R]
/*
{
     def power(base: R, exp: R): R
     def nRoot(base: R, n: R)(implicit r: linalg.Number[R]): R = power(base, r.divide(r.one, n))
     def squareRoot(base: R)(implicit r: linalg.Number[R]): R = nRoot(base, r.two)
}
*/

object Root {
     @inline final def apply[R](implicit ev: Root[R]): Root[R] = ev
}

package linalg.matrix

/**
  *
  */

import linalg._




trait MatrixLike[M, F] extends SetVecLike[M, F] {

     //val identity: M //rule: identity is dim-by-dim wide (so numrows by numrows wide)
     // so that we can test if set spans a space

     def power(m: M, exp: F): M //use linear algebra formula for matrix powers
     def inverse(m: M): M
     def transpose(m: M): M
     def conjugateTranspose(m: M): M
     def adjoint(m: M): M
     def cofactor(m: M): M
     def minor(m: M): M
     def minor(m: M, rowIndex: Int, colIndex: Int): F
     def determinant(m: M): M
     def trace(m: M): F
}

object MatrixLike {
     @inline final def apply[M,F](implicit ev: MatrixLike[M,F]): MatrixLike[M,F] = ev
}

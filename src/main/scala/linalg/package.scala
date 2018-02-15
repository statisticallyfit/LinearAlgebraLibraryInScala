import linalg.util.Show

/**
  *
  */
package object linalg {

     //note - what is the point of this structure? Was wondering if thi sis what makes
     // note the Eq[A] pattern be blue instead of pink?
     //note ANSWER: YES!!!

     //kernel stuff
     type Number[N] = linalg.kernel.Number[N]
     type Absolute[N, R] = linalg.kernel.Absolute[N, R]
     type Abs[A] = linalg.kernel.Abs[A]
     type NRoot[N, R] = linalg.kernel.NRoot[N, R]
     type Root[R] = linalg.kernel.Root[R]
     type Equality[E] = linalg.kernel.Equality[E]
     type RealNumber[R] = linalg.kernel.RealNumber[R]
     type Trig[T] = linalg.kernel.Trig[T]
     val Number = linalg.kernel.Number
     val Absolute = linalg.kernel.Absolute
     val Abs = linalg.kernel.Abs
     val NRoot = linalg.kernel.NRoot
     val Root = linalg.kernel.Root
     val Equality = linalg.kernel.Equality
     val RealNumber = linalg.kernel.RealNumber
     val Trig = linalg.kernel.Trig


     //util stuff
     type Show[S] = linalg.util.Show[S]
     val Show = linalg.util.Show

     //theory stuff
     type Basis[B, F] = linalg.theory.basis.Basis[B, F]
     type Dimension[V] = linalg.theory.basis.Dimension[V]
     type LinearIndependence[V, F] = linalg.theory.basis.LinearIndependence[V, F]
     type Rank[V, F] = linalg.theory.basis.Rank[V, F]
     type Span[V, F] = linalg.theory.basis.Span[V, F]
     type Transformation[V, W] = linalg.theory.basis.Transformation[V, W]
     val Basis = linalg.theory.basis.Basis
     val Dimension = linalg.theory.basis.Dimension
     val LinearIndependence = linalg.theory.basis.LinearIndependence
     val Rank = linalg.theory.basis.Rank
     val Span = linalg.theory.basis.Span
     val Transformation = linalg.theory.basis.Transformation

     type VectorSpace[V, F] = linalg.theory.space.VectorSpace[V, F]
     type Subspace[S, F] = linalg.theory.space.Subspace[S, F]
     type ColumnSpace[C, F] = linalg.theory.space.ColumnSpace[C, F]
     type RowSpace[R, F] = linalg.theory.space.RowSpace[R, F]
     type NullSpace[N, F] = linalg.theory.space.NullSpace[N, F]
     type InnerProductSpace[I, F] = linalg.theory.space.InnerProductSpace[I, F]
     type NormedVectorSpace[N, F] = linalg.theory.space.NormedVectorSpace[N, F]
     type OrthogonalSpace[O, F] = linalg.theory.space.OrthogonalSpace[O, F]
     type OrthonormalSpace[O, F] = linalg.theory.space.OrthonormalSpace[O, F]
     type CoordinateSpace[C, F] = linalg.theory.space.CoordinateSpace[C, F]
     type ContinuousFunctionSpace[C, F] = linalg.theory.space.ContinuousFunctionSpace[C, F]
     type PolynomialSpace[P, F] = linalg.theory.space.PolynomialSpace[P, F]
     type BanachSpace[B, F] = linalg.theory.space.BanachSpace[B, F]
     type HilbertSpace[H, F] = linalg.theory.space.HilbertSpace[H, F]
     val VectorSpace = linalg.theory.space.VectorSpace
     val Subspace = linalg.theory.space.Subspace
     val ColumnSpace = linalg.theory.space.ColumnSpace
     val RowSpace = linalg.theory.space.RowSpace
     val NullSpace = linalg.theory.space.NullSpace
     val InnerProductSpace = linalg.theory.space.InnerProductSpace
     val NormedVectorSpace = linalg.theory.space.NormedVectorSpace
     val OrthogonalSpace = linalg.theory.space.OrthogonalSpace
     val OrthonormalSpace = linalg.theory.space.OrthonormalSpace
     val CoordinateSpace = linalg.theory.space.CoordinateSpace
     val ContinuousFunctionSpace = linalg.theory.space.ContinuousFunctionSpace
     val PolynomialSpace = linalg.theory.space.PolynomialSpace
     val BanachSpace = linalg.theory.space.BanachSpace
     val HilbertSpace = linalg.theory.space.HilbertSpace

     //todo not doing stuff in Category since I might want to use Cats category stuff later!

     //matrix stuff //note - no need to write vec stuff like vectorlike.
     type MatrixLike[M, F] = linalg.matrix.MatrixLike[M, F]
     type LinearSystem[S, F] = linalg.matrix.LinearSystem[S, F]
     val MatrixLike = linalg.matrix.MatrixLike
     val LinearSystem = linalg.matrix.LinearSystem

}

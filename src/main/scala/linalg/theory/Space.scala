package linalg.theory


import linalg.numeric._

import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.universe._

//------------------------------------------------------------------------------------------------------
/**
  * A vector space is a set V together with two binary operations that combine
  * two entities to yield a third, called vector addition and scalar multiplication.
  * Vector spaces fall into two categories: A vector space V is said to be ﬁnite-dimensional if there is a finite set
  * of vectors in V that spans V and is said to be inﬁnite-dimensional if no such set exists.
  *
  * Laws:
  * (i) a + b = b + a                   --- commutativity, vector addition
  * (ii) (a + b) + c = a + (b + c)      --- associativity, vector addition
  * (iii) k(a + b) = ka + kb            --- distributivity, vector addition
  * (iv) k(cu) = (kc) u                 --- associativity, scalar multiplication
  * (v) k + c)u = ku + cu               --- distributivity, scalars:
  *
 */
trait VectorSpace[V, F] extends AbelianGroup[V] with Monoid[V] {

     val zero: V
     val one: V
     //def zero(n: Int): V //neutral element: 0 + u = u
     //def one(n: Int): V // identity element: 1*u = u

     def plus(v: V, w: V): V //addition => u + v
     def negate(v: V): V //additive inverse => u + (-u) = 0
     def scale(v: V, constant: F): V //scalar multiplication: ku
}


//TODO tomorrow look at spire's methods: https://github.com/non/spire/blob/f86dfda4fb3029f23c023c940ea61dde51e4a0f1/core/shared/src/main/scala/spire/algebra/InnerProductSpace.scala
//TODO next: make the testing things in Discipline (grouplaws, innerprodspace laws, vecspace laws ...etc)
/**
  * An inner product on a real vector space V is an operation <,> which assigns
  * a unique real number to each pair of vectors, u, and v, which satisfies the
  * following axioms for all vectors u,v,w in V and all scalars k.
  *
  * Laws:
  * (i) < u, v> = < v, u >                   --- commutative law
  * (ii) < u+v, w > = < u,w > + < v,w >      --- distributive law
  * (iii) < ku, v > = k< u,v >               --- taking out scalar k
  * (iv) < u,u > >= 0 and we have < u,u> = 0 if and only if u = 0
  *                                          --- (means the inner product is zero or positive)
  *
  */
trait InnerProductSpace[I, F] extends VectorSpace[I, F] { self =>

     def innerProduct(i1: I, i2: I): F
     def dotProduct(i1: I, i2: I): F = innerProduct(i1, i2)

     def normed(implicit rootEv: Root[F,F]): NormedVectorSpace[I, F] =
          new NormedInnerProductSpace[I, F] {
               def space = self
               def nroot: Root[F,F] = rootEv
          }
}

object InnerProductSpace {
     //todo meaning of final?
     final def apply[I, R](implicit inner: InnerProductSpace[I, R]): InnerProductSpace[I, R] = inner
}

private[theory] trait NormedInnerProductSpace[V, F] extends NormedVectorSpace[V, F] {
     def space: InnerProductSpace[V, F]
     def scalar: Field[F] = space.scalar
     def nroot: Root[F,F]

     def zero: V = space.zero
}
//-------------------

trait NormedVectorSpace[V, F] extends VectorSpace[V, F] {

     //this: Field[F] =>

     def norm(n: V): F
     def normalize(n: V): V
     def isNormalized(n: V): Boolean
}

/**
  * A Banach space, B, is a complete normed vector space such that every Cauchy sequence (with respect
  * to the metric d(x, y) = |x - y|) in B has a limit in B.
  */
trait BanachSpace[B, F] extends /*with Field[F]*/  NormedVectorSpace[B, F] {

     //this: Field[F] =>

     // |⋅| : B → F
     //norm assigns a strictly positive length or size to all vectors in the vector space, other than the zero vector.
     def norm(v: B): F //note calculates the 2-norm
     def normalize(v: B): B
     def isNormalized(v: B): Boolean
}


/**
  * A Hilbert space is an inner product space, an abstract vector space in which distances and angles
  * can be measured. It is also "complete", meaning that if a sequence of vectors is Cauchy, then it
  * converges to some limit in the space.
  */
trait HilbertSpace[H, F] extends InnerProductSpace[H, F] {

     //this: Field[F] =>
     //this: Field[F] with Dimension[H] with BanachSpace[H, F] =>

     //∠ : H × H → F
     // Inner product formalizes the geometrical notions such as the length of a vector and the angle between two vectors.
     def angle(that: H): F

     // <⋅,⋅> : H × H → F
     // Inner product formalizes the geometrical notions such as the length of a vector and the angle between two vectors.
     def dotProduct(that: H): F
}


//------------------------------------------------------------------------------------------------------

// TODO look at methods in linear algbera spire
trait LinearSubspace[S, F] extends VectorSpace[S, F]{

     //this: Field[F] =>
}

/**
  * A non-empty subset S of vector space V is a subspace of V if it also
  * satisfies the ten axioms of a vector space.
  */
trait Subspace[S, F] extends VectorSpace[S, F] {

     //todo corect? Check that subset is indeed subset of gen, only thing, since vecspace axioms are satisfied
     // automatically when extending.
     def isSubspaceOf(aSubset: S, generalVecSpace: VectorSpace[S,F]): Boolean
}


trait Basis[B, F] extends VectorSpace[B, F] /*extends Orthonormal[V] with Span[Basis[V, F], F]*/ {

    // this: Field[F] =>
     //this: VectorSpace[B, N] with Span[B, N] with LinearIndependence[B, N] =>

     //note ifvecset cols are linearly independent, then the vecset is a basis for vecpsace V^n,
     // if not return None.
     // which means this vecset is not a basis for the V^n vecspace. prereq is isBasisOfSpaceWith function
     def basis(): Option[B]
     //def isBasisOfSpaceWith(dim: Int): Boolean //todo do we really need this?
}

//todo need to provide V, F or just V?
trait Dimension[V]{

     this: VectorSpace[V, _] =>

     def dimension(vectorSpace: V): Int
}





//------------------------------------------------------------------------------------------------------
////todo only problem is that we can't constrain V to be vecspace because if we do, then banacspace extends normal
//// won't work
////note: the trick to putting self types: for trait A, its self type this: B is such that B is higher than A and such
//// note -- A is a type of B or is related.
//// example: anything that implements Normal[B, F] must also implement vecspace and banachspace because these two use
//// normal.
//
//
//TODO //todo - started showing type parameter type is Field ....????


//note: if using self-type this: Field[F] then the relation is HAS-A between the trait and the self-typer.
//note: but if using inheritance Field[F] then relation is IS-A between the trait and F.
//note: relation is still IS-A for the class that mixes in the overall trait.
//source: https://softwareengineering.stackexchange.com/questions/219038/what-is-the-difference-between-self-types-and-trait-inheritance-in-scala
// https://stackoverflow.com/questions/2224932/difference-between-trait-inheritance-and-self-type-annotation
trait Orthogonal[V, F] extends VectorSpace[V, F] /*with Field[F] */{
     //this: Field[F] =>

     def isOrthogonal(v: V): Boolean
     def areOrthogonal(v1: V, v2: V): Boolean
     def orthogonalize(v: V): V
}

trait Orthonormal[V, F] extends Orthogonal[V, F] with NormedVectorSpace[V, F]  {

     //this: Field[F] =>
     def orthonormalize(v: V): V = normalize( orthogonalize(v) )
}


//------------------------------------------------------------------------------------------------------


//todo

//class RowSpace[N <: Number[N]: TypeTag](vset: VectorSet[N])
//     extends AbelianGroup[RowSpace[N]] //with Ring[RowSpace[N]] with Monoid[RowSpace[N]]
//          with VectorSpace[RowSpace[N], N] with BanachSpace[RowSpace[N], N]
//          with LinearIndependence[RowSpace[N], N]
//          with BasisSpace[RowSpace[N], N] with Dimension[RowSpace[N], N]
//          with Orthonormal[RowSpace[N], N] {
//
//     private val rowBasis: Matrix[N] = makeBasisMatrix()
//
//     def getVectorSet(): VectorSet[N] = vset
//     def getMatrix(): Matrix[N] = vset //note implicit here
//
//     def tempGetBasis() = rowBasis
//     def isInRowSpace(b: Vector[N]): Boolean = rowBasis.isSpanned(new VectorSet(b))
//     def getRowSpaceVector(coefs: Vector[N]): Vector[N] = rowBasis.getVectorInSpace(coefs)
//     def dimension(): Int = rowBasis.numRows
//     def isBasisOfSpaceWith(dim: Int): Boolean = rowBasis.spansSpaceWith(dim) && rowBasis.linearlyIndependent()
//     def basis(): RowSpace[N] = new RowSpace(makeBasisMatrix())
//     private def makeBasisMatrix(): Matrix[N] = getMatrix().reducedRowEchelon().transpose()
//     def linearlyIndependent(): Boolean = getMatrix().linearlyIndependent() //note: is rowspace cols linindep?
//
//     def norm(): N = rowBasis.norm()
//     def normalize(): RowSpace[N] = new RowSpace(rowBasis.normalize())
//     def isNormalized(): Boolean = rowBasis.isNormalized()
//     def orthogonalize(): RowSpace[N] = new RowSpace(rowBasis.orthogonalize())
//     def isOrthogonal(): Boolean = rowBasis.isOrthogonal()
//     def isOrthogonalTo(that: RowSpace[N]): Boolean = rowBasis.isOrthogonalTo(that.getMatrix())
//
//     def ZERO(): RowSpace[N] = new RowSpace(getVectorSet().ZERO())
//     def +(that: RowSpace[N]): RowSpace[N] = new RowSpace(getVectorSet() + that.getVectorSet()) //todo correct?
//     def -(that: RowSpace[N]): RowSpace[N] = new RowSpace(getVectorSet() - that.getVectorSet()) //todo correct?
//     def opposite(): RowSpace[N] = new RowSpace(getVectorSet().opposite())
//     def scale(factor: N): RowSpace[N] = new RowSpace(getVectorSet().scale(factor))
//     def scale(factor: Double): RowSpace[N] = new RowSpace(getVectorSet().scale(factor))
//
//     override def toString: String = getVectorSet().toString
//}
//
//
//
//class ColumnSpace[N <: Number[N]: TypeTag](vset: VectorSet[N])
//     extends AbelianGroup[ColumnSpace[N]] //with Ring[ColumnSpace[N]] with Monoid[ColumnSpace[N]]
//          with VectorSpace[ColumnSpace[N], N] with BanachSpace[ColumnSpace[N], N]
//          with LinearIndependence[ColumnSpace[N], N]
//          with BasisSpace[ColumnSpace[N], N] with Dimension[ColumnSpace[N], N]
//          with Orthonormal[ColumnSpace[N], N] {
//
//
//     private val colBasis: Matrix[N] = makeBasisMatrix()
//
//     def getVectorSet(): VectorSet[N] = vset
//     def getMatrix(): Matrix[N] = vset
//
//     def isInColSpace(b: Vector[N]): Boolean = colBasis.isSpanned(new VectorSet(b))
//     def getColumnSpaceVector(coefs: Vector[N]): Vector[N] = colBasis.getVectorInSpace(coefs)
//     def dimension(): Int = colBasis.numCols
//     def linearlyIndependent(): Boolean = getMatrix().linearlyIndependent() //note: is rowspace cols linindep?
//     def isBasisOfSpaceWith(dim: Int): Boolean = colBasis.spansSpaceWith(dim) && colBasis.linearlyIndependent()
//     //todo is this also called the image basis? matrixLib seems to return rows for rref pivot cols, not cols for
//     // rref pivot cols (so matrixLib seems incorrect)
//     // See:
//     // http://math.stackexchange.com/questions/1286677/finding-basis-for-image-of-linear-transformation
//     // note: best: https://crazyproject.wordpress.com/2011/07/17/find-bases-for-the-image-and-kernel-of-a-given-linear-transformation-2/
//     // http://math.stackexchange.com/questions/895406/how-to-find-a-basis-of-an-image-of-a-linear-transformation
//     def basis(): ColumnSpace[N] = new ColumnSpace(colBasis)
//     private def makeBasisMatrix(): Matrix[N]={
//          val rref: Matrix[N] = getMatrix().reducedRowEchelon()
//          val freeColIndices: Array[Int] = Util.GenOps.getIndicesOfFreeColumns(rref)
//          val colBasisIndices: Array[Int] =
//               (0 until rref.numCols).filterNot(i => freeColIndices.contains(i)).toArray
//          Matrix(colBasisIndices.map(c => getMatrix().getCol(c)):_*)
//     }
//
//     def norm(): N = colBasis.norm()
//     def normalize(): ColumnSpace[N] = new ColumnSpace(colBasis.normalize())
//     def isNormalized(): Boolean = colBasis.isNormalized()
//     def orthogonalize(): ColumnSpace[N] = new ColumnSpace(colBasis.orthogonalize())
//     def isOrthogonal(): Boolean = colBasis.isOrthogonal()
//     def isOrthogonalTo(that: ColumnSpace[N]): Boolean = colBasis.isOrthogonalTo(that.getMatrix())
//
//     def ZERO(): ColumnSpace[N] = new ColumnSpace(getVectorSet().ZERO())
//     def +(that: ColumnSpace[N]): ColumnSpace[N] = new ColumnSpace(getVectorSet() + that.getVectorSet()) //todo correct?
//     def -(that: ColumnSpace[N]): ColumnSpace[N] = new ColumnSpace(getVectorSet() - that.getVectorSet()) //todo correct?
//     def opposite(): ColumnSpace[N] = new ColumnSpace(getVectorSet().opposite())
//     def scale(factor: Double): ColumnSpace[N] = new ColumnSpace(getVectorSet().scale(factor))
//     def scale(factor: N): ColumnSpace[N] = new ColumnSpace(getVectorSet().scale(factor))
//
//     override def toString: String = getVectorSet().toString
//}
//
//
//
//class NullSpace[N <: Number[N]: TypeTag](vset: VectorSet[N])
//     extends AbelianGroup[NullSpace[N]] //with Ring[NullSpace[N]] with Monoid[NullSpace[N]]
//          with VectorSpace[NullSpace[N], N] with BanachSpace[NullSpace[N], N]
//          with LinearIndependence[NullSpace[N], N]
//          with BasisSpace[NullSpace[N], N] with Dimension[NullSpace[N], N]
//          with Orthonormal[NullSpace[N], N] {
//
//     private val kernelMat: Matrix[N] = makeBasisMatrix()
//
//     def getVectorSet(): VectorSet[N] = vset
//     def getMatrix(): Matrix[N] = vset
//
//     def isInNullSpace(b: Vector[N]): Boolean = kernelMat.isSpanned(new VectorSet(b))
//     def getVectorInNullSpace(coefs: Vector[N]): Vector[N] = kernelMat.getVectorInSpace(coefs)
//     def nullity(): Int = dimension()
//     def dimension(): Int = kernelMat.numCols
//     def linearlyIndependent(): Boolean = getMatrix().linearlyIndependent() //note: is rowspace cols linindep?
//     def isBasisOfSpaceWith(dim: Int): Boolean = kernelMat.spansSpaceWith(dim) && kernelMat.linearlyIndependent()
//     def basis(): NullSpace[N] = kernel()
//     //todo will this always be an option? Isn't it always guaranteed to at least always have  unique OR infinite
//     // solution?
//     def kernel(): NullSpace[N] = new NullSpace(kernelMat)
//     private def makeBasisMatrix(): Matrix[N] = {
//          val zeroVec: Vector[N] = Vector.ZERO[N](getVectorSet().numRows)
//          val op = new AugmentedMatrix(getVectorSet(), zeroVec).solve()
//          Matrix.fromLists[N](op.get.getColumns().map(vec => vec.toList.map(e => e.asInstanceOf[N])):_*)
//     }
//
//     def norm(): N = kernelMat.norm()
//     def normalize(): NullSpace[N] = new NullSpace(kernelMat.normalize())
//     def isNormalized(): Boolean = kernelMat.isNormalized()
//     def orthogonalize(): NullSpace[N] = new NullSpace(kernelMat.orthogonalize())
//     def isOrthogonal(): Boolean = kernelMat.isOrthogonal()
//     def isOrthogonalTo(that: NullSpace[N]): Boolean = kernelMat.isOrthogonalTo(that.getMatrix())
//
//     def ZERO(): NullSpace[N] = new NullSpace(getVectorSet().ZERO())
//     def +(that: NullSpace[N]): NullSpace[N] = new NullSpace(getVectorSet() + that.getVectorSet()) //todo correct?
//     def -(that: NullSpace[N]): NullSpace[N] = new NullSpace(getVectorSet() - that.getVectorSet()) //todo correct?
//     def opposite(): NullSpace[N] = new NullSpace(getVectorSet().opposite())
//     def scale(factor: Double): NullSpace[N] = new NullSpace(getVectorSet().scale(factor))
//     def scale(factor: N): NullSpace[N] = new NullSpace(getVectorSet().scale(factor))
//
//     override def toString: String = getVectorSet().toString
//}

trait LinearTransformation

//trait Rank[T <: LinearTransformation]


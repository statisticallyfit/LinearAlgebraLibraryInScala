package linalg.theory.space

//import linalg.kernel.{RealNumber, NRoot, Trig}
import linalg._


/**
  * A Hilbert space is an inner product space, an abstract vector space in which distances and angles
  * can be measured. It is also "complete", meaning that if a sequence of vectors is Cauchy, then it
  * converges to some limit in the space.
  *
  */
trait HilbertSpace[H, F] extends InnerProductSpace[H, F] {
     //∠ : H × H → F
     // Inner product formalizes the geometrical notions such as the length of a vector and the angle between two vectors.
     def angle[R:RealNumber](v: H, w: H)(implicit t: Trig[F], f: linalg.Field[F], r: Root[F,R]): F

     // <⋅,⋅> : H × H → F
     // Inner product formalizes the geometrical notions such as the length of a vector and the angle between two vectors.
     //def dotProduct(that: H): F
}


object HilbertSpace {
     @inline final def apply[H, F](implicit ev: HilbertSpace[H, F]): HilbertSpace[H, F] = ev
}

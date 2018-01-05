package theory


//------------------------------------------------------------------------------------------------------

trait Group[G]
/**
  * A monoid is a group with a binary operation (×), satisfying the group axioms:
  *       closure
  *       associativity
  *       existence of multiplicative identity
  * @tparam G
  */
trait Monoid[G] extends Group[G]


/**
  * An Abelian group is a group with a binary additive operation (+),
  * satisfying the group axioms:
  *       closure
  *       associativity
  *       existence of additive identity
  *       existence of additive opposite
  *       commutativity of addition
  */
trait AbelianGroup[G] extends Group[G] {

     // the associative operations
     def +(that: G): G
     // For each a in G, there exists an element b in G such that a + b = b + a = 0.
     def inverse(): G
     // zero is identity for abelian: a + 0 = a
     protected def zero: G
}

/**
  * A ring is a set R equipped with two binary operations called addition and
  * multiplication:
          + : R × R → R
          and
          ⋅ : R × R → R
  * To qualify as a ring, the set and two operations, (R, +, ⋅), must satisfy
  * the requirements known as the ring axioms.
  */
trait Ring[R] extends AbelianGroup[R] with Monoid[R] {

     //methods inherited: add,subtract,opposite,identity
     def *(that: R): R
}


/**
  * As an algebraic structure, every field is a ring, but not every ring is a field.
  * That is, it has the notion of addition, subtraction, multiplication,
  * satisfying certain axioms. The most important difference is that a field
  * allows for division (though not division by zero), while a ring may not
  * possess a multiplicative inverse. In addition, the multiplication operation
  * in a field is required to be commutative.
  */
trait Field[F/* <: Field[F]*/] extends AbelianGroup[F] with Ring[F] with Monoid[F] {

     //methods inherited: add,subtract,opposite,identity

     //That is the same as this.multiply(that.inverse())
     def /(that: F): F
     //For each a in F, there exists an element b in F such that a × b = b × a = 1.
     def inverse(): F

     protected def one: F
}

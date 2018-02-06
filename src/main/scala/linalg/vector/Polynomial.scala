package linalg.vector


import linalg.numeric._

/**
  *
  */

//note: can make it parametrized by Number if you like (in case you want Complex type too)

class Polynomial[R: RealLike](ps: R*) extends Vector[R](ps:_*) {

}

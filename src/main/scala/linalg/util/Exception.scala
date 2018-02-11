package linalg.util

/**
  *
  */
object Exception {

     /**
       * Thrown when matrix or vectors are not same size for addition etc.
       */
     case class VectorLikeSizeException(message: String) extends Exception(message)
}

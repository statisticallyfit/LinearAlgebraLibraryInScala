package linalg.util

/**
  *
  */
trait ExceptionOps {

     /**
       * Thrown when matrix or vectors are not same size for addition etc.
       */
     case class VectorLikeSizeException(message: String) extends Exception(message)
     case class MatrixLikeSizeException(message: String) extends Exception(message)
}

import linalg.numeric._

case class Vec[N: Number](elems: N*){
     //def elems_=(s: Seq[N]) = elems = s
     def copy(es:N*): Vec[N] ={
          Vec(es:_*)
     }
     def copy(es: Seq[N]): Vec[N] = Vec(es:_*)
     def copy(): Vec[N] = Vec(elems:_*)

     def set(index: Int)(value: N): Unit ={
          val (p1, p2) = elems.splitAt(index)
          val newElements = (p1 :+ value) ++ p2.tail
          this = this.copy(newElements)
     }
}
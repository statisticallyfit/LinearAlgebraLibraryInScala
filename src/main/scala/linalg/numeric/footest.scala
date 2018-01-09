
object footest {
     //class LowerCase(val s: String)

     implicit case class LowerCase(s: String) {
          // Use `equals`, not `==`
          override def equals(that: Any) = that match {
               case t: LowerCase => t.s.equalsIgnoreCase(this.s)
               case _ => false
          }

          override def toString() = s
     }

     def main(args: Array[String]) {
          val a = new LowerCase("a")
          val b = new LowerCase("b")
          println(a)
          println(a equals b)
     }
     /*trait Printer[T] {
          def print(t: T): String
     }

     class Cake(val n: Int)

     implicit val sp: Printer[Int] = new Printer[Int] {
          def print(i :Int) = i.toString
     }
     implicit val cakePrint: Printer[Cake] = new Printer[Cake] {
          def print(c: Cake) = s"!##!!${c.n}!!##!"
     }

     def foo[T](t: T)(implicit p: Printer[T]) = p.print(t)


     def main(args: Array[String]) {
          val res = foo(3)
          // val res = foo(false)
          println(s"res: ${res}")

          val cakeres = new Cake(14)
          println(s"cake: ${cakeres}")
     }*/
}
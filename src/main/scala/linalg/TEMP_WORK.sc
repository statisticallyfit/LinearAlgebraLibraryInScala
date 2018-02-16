trait Food[F] {
     def food(f: F): String
}

trait Fruit[F] extends Food[F] {
     def fruit(f: F): String
}

case class Melon[M](melon: M)

trait MelonIsFood[M] extends Food[Melon[M]] {
     def food(f: Melon[M]): String = f.toString + " in food trait"
}

class MelonIsFruit[M] extends Fruit[Melon[M]] with MelonIsFood[M] {
     def fruit(f: Melon[M]) = f.toString + "in fruit class"
}
object Fruit {
     implicit val MelonIsFruit = new MelonIsFruit
}


/*trait FoodInstances {

     implicit def melonIsFood[M] = new Food[Melon[M]]{
          def food(m: Melon[M]) = m.toString + "in food trait"
     }
}*/

/*
trait FruitInstances extends FoodInstances {
     implicit def melonIsFruit[M] = new Fruit[Melon[M]]{
          def fruit(m: Melon[M]) = m.toString + "in fruit trait"
     }
}*/

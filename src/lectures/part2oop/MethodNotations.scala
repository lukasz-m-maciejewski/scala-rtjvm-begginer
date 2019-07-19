package lectures.part2oop

object MethodNotations extends App {

  class Person(val name: String, favoriteMovie: String) {
    def likes(movie: String): Boolean = favoriteMovie == movie

    def hangsOutWith(person: Person): String = s"${this.name} hangs out with ${person.name}"

    def +(person: Person): String = hangsOutWith(person)

    def unary_!(): String = s"$name, what the fuck?!"

    def apply(n: Int): String = s"$name and $n"
  }

  val mary = new Person("Mary", "Jaws")
  println(mary.likes("Jaws"))
  println(mary likes "Jaws")

  val tom = new Person("Tom", "Dune")
  println(mary hangsOutWith tom)

  println(mary + tom)

  println(!tom)

  println(tom(3))
}

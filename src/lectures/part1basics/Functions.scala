package lectures.part1basics

object Functions extends App {
  def aFunction(a : String, b : Int): String =
    a + " " + b

  println(aFunction("Goodbye", 42))
}

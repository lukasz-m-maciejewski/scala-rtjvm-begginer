package lectures.part3functional

object WhatsAFunction extends App {
  val stringToIntConverter = new ((String) => Int) {
    override def apply(v1: String): Int = v1.toInt
  }

  println(stringToIntConverter("3") + 4)

  val addConst: ((Int) => (Int => Int)) = new Function1[Int, Function1[Int, Int]] {
    override def apply(c: Int): Int => Int = {
      x => x + c
    }
  }

  val addConst2: ((Int) => (Int => Int)) = (c: Int) => {
    x => x + c
  }

  val add7: (Int => Int) = addConst(7)

  println(add7(3))

}

package lectures.part3functional

object MapFlatMapFilterFor extends App {
  val list = List(1, 2, 3)
  println(list.head)
  println(list.tail)

  println(list.map(_ + 1))

  println(list.filter(_ % 2 == 0))
}

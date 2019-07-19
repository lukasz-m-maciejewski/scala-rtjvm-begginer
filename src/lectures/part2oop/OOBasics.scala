package lectures.part2oop

object OOBasics extends App {
  val person = new Person("John", 26)
  println(person)

  val writer = new Writer("Adam", "Mickiewicz", 1666)
  println(writer.fullName)

  val tolstoy = new Writer("Leo", "Tolstoy", 1828)
  val warAndPiece = new Novel("War and Piece", 1869, tolstoy)
  println(warAndPiece.toStr())

  val reEd = warAndPiece.copy(1990)
  println(reEd.toStr())
}

class Person(name: String, age: Int) {

}

class Writer(name: String, surname: String, val yearOfBirth: Int) {
  def fullName(): String = name + " " + surname

  def toStr(): String = fullName() + " " + yearOfBirth
}

class Novel(title: String, yearOfRelease: Int, author: Writer) {
  def isWrittenBy(): Writer = author

  def authorAge(): Int = yearOfRelease - author.yearOfBirth

  def copy(yearOfReRelease: Int): Novel = new Novel(title, yearOfReRelease, author)

  def toStr(): String = title + " " + yearOfRelease + " " + author.toStr()

}

class Counter(val count: Int) {

  def increment(): Counter = new Counter(count + 1)

  def increment(inc: Int): Counter = new Counter(count + inc)

  def decrement(): Counter = new Counter(count - 1)

  def decrement(dec: Int): Counter = new Counter(count - dec)

  def inc = {
    println("Incementing!")
    new Counter(count + 1)
  }

  def dec = {
    println("Decrementing!")
    new Counter(count - 1)
  }

  def inc(n: Int): Counter = {
    if (n <= 0) this
    else inc.inc(n - 1)
  }

  def dec(n: Int): Counter = {
    if (n <= 0) this
    else dec.dec(n - 1)
  }

}
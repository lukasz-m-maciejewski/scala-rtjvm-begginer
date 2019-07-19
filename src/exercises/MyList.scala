package exercises

object MyList extends App {
  val l = Empty
  val l1 = l.add(42)
  val l2 = l1.add(24)
  val l3 = l2.add(99)

  println(l3.toString())

  val lof: List[Int] = new Cons(1, new Cons(2, new Cons(3, Empty)))

  val los: List[String] = new Cons("a", new Cons("b", new Cons("c", new Cons("d", Empty))))

  println(lof)

  println(los)

  println(lof.map(x => x.toString))

  println(lof.filter(x => x % 2 == 1))

  println(lof.flatMap(x => new Cons(x, new Cons(x + 1, Empty))))

  lof.foreach(x => println(x))

  println(lof.sort((x, y) => x - y))
  println(lof.sort((x, y) => y - x))

  println(los.zipWith(lof, (s: String, i: Int) => s + i))

  println(lof.fold("a")((s: String, i: Int) => s + i))
}

object List {
  def flatten[A](lol: List[List[A]]): List[A] = {
    if (lol.isEmpty()) Empty
    else if (lol.head.isEmpty) flatten(lol.tail())
    else join(lol.head, flatten(lol.tail))
  }

  def reverseJoin[A](lhs: List[A], rhs: List[A]): List[A] = {
    if (lhs.isEmpty) rhs
    else reverseJoin(lhs.tail, rhs.add(lhs.head))
  }

  def reverse[A](l: List[A]): List[A] = reverseJoin(l, Empty)

  def join[A](lhs: List[A], rhs: List[A]): List[A] = reverseJoin(reverse(lhs), rhs)

  def curry[A, B, C](fun: (A, B) => C): A => B => C = (x: A) => ((y: B) => fun(x, y))

  def uncurry[A, B, C](fun: A => B => C): (A, B) => C = (x: A, y: B) => fun(x)(y)

  def compose[A, B, C](f: B => C, g: A => B): A => C = x => f(g(x))

  def andThen[A, B, C](f: A => B, g: B => C): A => C = x => g(f(x))
}

abstract class List[+A]() {
  def head(): A

  def tail(): List[A]

  def isEmpty(): Boolean

  def add[B >: A](n: B): List[B]

  def printElements: String

  override def toString: String = "[" + this.printElements + "]"

  def map[B](transformer: A => B): List[B] = {
    if (this.isEmpty()) Empty
    else new Cons(transformer(head), this.tail.map(transformer))
  }

  def filter(predicate: A => Boolean): List[A] = {
    if (this.isEmpty) Empty
    else if (predicate(this.head)) new Cons(this.head, this.tail.filter(predicate))
    else this.tail.filter(predicate)
  }

  def ++[B >: A](list: List[B]): List[B]

  def flatMap[B](transformer: A => List[B]): List[B]

  def foreach(action: A => Unit)

  def sort(comparator: (A, A) => Int): List[A]

  def zipWith[B, C](that: List[B], zipper: (A, B) => C): List[C]

  def fold[B](init: B)(f: (B, A) => B): B
}

object Empty extends List[Nothing] {
  override def head(): Nothing = throw new NoSuchElementException

  override def tail: List[Nothing] = throw new NoSuchElementException

  override def isEmpty = true

  override def add[B >: Nothing](elem: B): List[B] = new Cons(elem, this)

  override def printElements: String = ""

  override def ++[B >: Nothing](list: List[B]): List[B] = list

  override def flatMap[B](transformer: Nothing => List[B]): List[B] = Empty

  override def foreach(action: Nothing => Unit): Unit = ()

  override def sort(comparator: (Nothing, Nothing) => Int): List[Nothing] = Empty

  override def zipWith[B, C](that: List[B], zipper: (Nothing, B) => C): List[Nothing] = Empty

  override def fold[B](init: B)(f: (B, Nothing) => B): B = init
}

class Cons[+A](val head: A, val tail: List[A]) extends List[A] {
  override def isEmpty(): Boolean = false

  override def add[B >: A](elem: B): List[B] = new Cons(elem, this)

  override def printElements: String = {
    if (tail.isEmpty()) "" + head
    else "" + head + ", " + tail.printElements
  }

  override def ++[B >: A](list: List[B]): List[B] = new Cons(this.head, this.tail ++ list)

  override def flatMap[B](transformer: A => List[B]): List[B] = {
    transformer(this.head) ++ this.tail.flatMap(transformer)
  }

  override def foreach(action: A => Unit): Unit = {
    action(this.head)
    this.tail.foreach(action)
  }

  override def sort(comparator: (A, A) => Int): List[A] = {
    if (this.tail.isEmpty()) this
    else {
      val lt = (x : A) => (comparator(this.head, x) < 0)
      val eq = (x : A) => (comparator(this.head, x) == 0)
      val gt = (x : A) => (comparator(this.head, x) > 0)
      val elems_lt = this.filter(lt)
      val elems_gt = this.filter(gt)
      (elems_lt.sort(comparator)) ++ (this.filter(eq)) ++ (elems_gt.sort(comparator))
    }
  }

  override def zipWith[B, C](that: List[B], zipper: (A, B) => C): List[C] = {
    if (that.isEmpty()) Empty
    else new Cons[C](zipper(this.head, that.head), this.tail.zipWith(that.tail, zipper))
  }

  override def fold[B](init: B)(f: (B, A) => B): B = this.tail.fold(f(init, this.head))(f)
}

package lectures.part1basics

object Recursion extends App {
  def factorial(n: Int): Int =
    if (n <= 1) 1
    else n * factorial(n - 1)

  def trFactorial(n: Int): BigInt = {
    def fHelper(x: Int, accumulator: BigInt): BigInt =
      if (x <= 1) accumulator
      else fHelper(x - 1, x * accumulator)

    fHelper(n, 1)
  }

  println(factorial(10))
  println(trFactorial(10))
  println(trFactorial(5000))
}

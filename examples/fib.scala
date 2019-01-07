import SubstitutionMacros._

object Fib extends App {
  implicit val pr = new SubstitutionMacros.Printer
  substitute {
    def count(x: Int): Int = if(x == 0) 0 else count(x - 1) + 1
    val x = count(2)
  }

  substitute {
    def fib(x: Int): Int = if(x <= 1) 1 else fib(x - 1) + fib(x - 2)
    val y = fib(4)
  }
}


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

  substitute {
    def init[A](lll: List[A]): List[A] = lll match {
      case Nil => throw new Exception("Nil has no init")
      case x :: Nil => Nil
      case x :: xs => x :: init(xs)
    }
    val xs = List(1, 2, 3, 4, 5)
    val ys = init(xs)
  }

  // TODO:
  // - inner functions
  // - function parameters with short names
  // - match/case
  // - more than one argument
  // - tail calls

  // Based on exercise 2.1
  substitute {
    def fib2(nnn: Int): Int = {
      @annotation.tailrec
      def loop(a: Int, b: Int, count: Int): Int = {
        if(count == 3) a + b
        else loop(b, a + b, count - 1)
      }
      if(nnn == 1) 0
      else if (nnn == 2) 1
      else loop(0, 1, nnn)
    }
    val x = fib2(4)
  }
}


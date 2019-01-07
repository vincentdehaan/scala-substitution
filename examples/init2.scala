import scala.util.{Try, Success}

import SubstitutionMacros._

object Init2 extends App {
  implicit val pr = new SubstitutionMacros.Printer
  substitute {
    def init[A](l: List[A]): List[A] = l match {
      case Nil => throw new Exception("Nil has no init")
      case x :: Nil => Nil
      case x :: xs => x :: init(xs)
    }
println(1)
    val xs = List(1, 2, 3, 4, 5)
    val ys = init(xs)
  }
  //val ys2 = printres(xs)(x => printargs(x)("init", y => printbody(y)("xxx match {...}", "xxx", init)))
}

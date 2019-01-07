import scala.util.{Try, Success}

//import SubstitutionMacros._

object Init extends App {
  def init[A](l: List[A])(implicit pr: Printer): List[A] = l match {
    case Nil => throw new Exception("Nil has no init")
    case x :: Nil => Nil
    case x :: xs => x :: printres(xs)(x => printargs(x)("init", y => printbody(y)("xxx match {...}", "xxx", init)))//init(xs) //printres(printargs("init", printbody("xxx match {...}", "xxx", init[A])))(xs)
  //init(xs)
  }

  /*def initmatch[A](l: List[A])(implicit pr: Printer): List[A] = printmatch[A](List(
    ({case Nil => true}, {case Nil => throw new Exception("nil has no init")}, _ => "Nil", _ => "throw"),
    ({case x :: Nil => true}, {case x :: Nil => Nil}, _ => "x :: Nil", _ => "Nil"),
    ({case x :: xs => true}, {case x :: xs => printres(printargs("init", printbody("xxx match {...}", "xxx", initmatch[A])))(xs)}, _ => "x :: xs", _ => "x :: init(xs)"
  )))(l)*/

  implicit val printer = new Printer

  val xs = List(1, 2, 3, 4, 5)
  val ys = init(xs)

  println(ys)

  def printargs[A1, B](arg1: A1)(fname: String, f: A1 => B)(implicit pr: Printer): B = {
    pr.indent += 1
    pr.print(s"$fname($arg1)")
    f(arg1)
  }

  def printbody[A1, B](arg1: A1)(body: String, substring: String, f: A1 => B)(implicit pr: Printer): B = {
    val substituted = body.replace(substring, arg1.toString)
    pr.print(substituted)
    f(arg1)
  }

  def printres[A1, B](arg: A1)(f: A1 => B)(implicit pr: Printer): B = {
    val r = f(arg)
    pr.print(r.toString)
    pr.indent -= 1
    r
  }

  def printmatch[A](cases: List[(List[A] => Boolean, List[A] => List[A], List[A] => String, List[A] => String)])(l: List[A])(implicit pr: Printer): List[A] = {
    cases.find(f => Try{f._1(l)}.isInstanceOf[Success[Boolean]]) match {
      case Some(ff) => {
        val casestring = "case " + ff._3(l) + " => " + ff._4(l)
        pr.print(s"$l match {$casestring}")
        ff._2(l)
      }
      case None => throw new Exception("Match error!")
    }
  }

  val ys2 = printargs(xs)("init",  init)
  println(ys2)

  //val ys3 = printbody(xs)("xxx match {...}", "xxx", x => printargs(x)("init", init))
  //val ys4 = printargs("init", printbody("xxx match {...}", "xxx", init))(xs)
  //println(ys3)
  //println(ys4)

println("----")
  val ys5 = printres(xs)(x => printargs(x)("init", y => printbody(y)("xxx match {...}", "xxx", init)))
}

class Printer {
  var indent: Int = 0;
  def print(s: String): Unit = println((" " * indent) + s.split("\n").mkString(" " * indent))
}
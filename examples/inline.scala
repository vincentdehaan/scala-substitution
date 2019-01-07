object Init extends App {
  @inline final def f(x: Int, y: Int) = x + y 

  val x = f(1, 2): @inline
}

class substitute extends scala.annotation.Annotation
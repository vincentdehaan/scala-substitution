object Init extends App {
  @inline final def f(x: Int) = x 

  val x = f(1): @inline
}
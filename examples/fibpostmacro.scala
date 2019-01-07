import SubstitutionMacros._

object Fib extends App {
  implicit val pr = new SubstitutionMacros.Printer

  def count(x: Int): Int = printall(x - 1)(count, "a", "b", "c") // if(x == 0) 0 else printall(x - 1)(count, "a", "b", "c") + 1
  val x = printall(4)(x.==, "a", "b", "c")
}
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context
import scala.util.{Success, Try}
import scala.collection.mutable

object SubstitutionMacros {
  def substitute[A](expr: A): Unit = macro substituteImpl[A]

  def substituteImpl[A](c: Context)(expr: c.Expr[A]): c.Expr[A] = {
    import c.universe._

    val defs = mutable.Map[String, String]()

    object definitionSearcher extends Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case q"def $tname($arg1): $tpe = $expr" => // TODO: why does this not match?
          println(s"${tname.toString} = ${expr.toString}")
          defs(tname.toString) = expr.toString
          tree
        case _ => super.transform(tree)
      }
    }

    object substitionTransformer extends Transformer {
      val defTable = scala.collection.mutable.Map[String, String]()
      override def transform(tree: Tree): Tree = tree match {
          case q"$f($arg1)" if !f.toString.contains(".") => // TODO: get rid of this ugly workardound!
            val t = q"""
                        val defs = Map[String, String]()
             printall($arg1)($f, ${f.toString}, ${defs(f.toString)}, ${arg1.toString})"""
           c.typecheck(t) match {
             case Block(_, print) => print
           }

          case _ => super.transform(tree)
      }
    }
    val d = definitionSearcher.transform(expr.tree) // Do we need to save this?

    val t = substitionTransformer.transform(expr.tree)
    println(substitionTransformer.defTable)
    val defTableExpr = c.typecheck(
      Block(q"""
         val defs = scala.collection.mutable.Map[String, String]()
       """ :: t :: substitionTransformer.defTable.map(df => q"defs(${df._1}) = ${df._2}").toList, q"println(5)")
    )
    println(showRaw(defTableExpr))
    c.Expr[A](defTableExpr)
  }

  class Printer {
    var indent: Int = 0;
    def print(s: String): Unit = println((" " * indent) + s.split("\n").mkString(" " * indent))
  }

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

  def printall[A1, B](arg1: A1)(f: A1 => B, fname: String, body: String, substring: String)(implicit pr: Printer): B =
    printres(arg1)(x => printargs(x)(fname, y => printbody(y)(body, substring, f)))


  // TODO
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
}
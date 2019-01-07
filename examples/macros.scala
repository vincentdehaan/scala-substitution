import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context
import scala.util.{Success, Try}
import scala.collection.mutable

object SubstitutionMacros {
  def substitute[A](expr: A): Unit = macro substituteImpl[A]

  def substituteImpl[A](c: Context)(expr: c.Expr[A]): c.Expr[A] = {
    import c.universe._

    val defs = mutable.Map[String, (String, String)]() // (defName, (defBody, firstArg))

    object definitionSearcher extends Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case q"def $tname[..$tp]($arg1): $tpe = $expr" =>
          arg1 match {
            case ValDef(_, TermName(arg1name), _, _) =>
              defs(tname.toString) = (expr.toString, arg1name)
              println(tname)
              tree
          }
        case _ => super.transform(tree)
      }
    }

    object substitionTransformer extends Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case q"$f[..$tp]($arg1)" if !f.toString.contains(".") => // TODO: get rid of this ugly workardound!
          val t = q"""
           printall($arg1)($f, ${f.toString}, ${defs(f.toString)._1}, ${defs(f.toString)._2})
            """
          c.typecheck(t)
        case _ => super.transform(tree)
      }
    }

    definitionSearcher.transform(expr.tree)

    val t = substitionTransformer.transform(expr.tree)
    c.Expr[A](t)
  }

  class Printer {
    var indent: Int = -1;
    var previndent: Int = -1;
    // TODO: reduce spaces in output
    def print(s: String): Unit = {
      val boxchar = if(previndent > indent) "\u2514" else ""
      println(("\u2502 " * indent) + boxchar + reduceWhitespace(s))
      previndent = indent
    }
  }

  def printargs[A1, B](arg1: A1)(fname: String, f: A1 => B)(implicit pr: Printer): B = {
    pr.indent += 1
    pr.print(s"$fname($arg1)")
    f(arg1)
  }

  def printbody[A1, B](arg1: A1)(body: String, substring: String, f: A1 => B)(implicit pr: Printer): B = {
    val substituted = body.replace(substring, arg1.toString)
    pr.print("= " + substituted)
    f(arg1)
  }

  def printres[A1, B](arg: A1)(f: A1 => B)(implicit pr: Printer): B = {
    val r = f(arg)
    pr.print("= " + r.toString)
    pr.indent -= 1
    r
  }

  def printall[A1, B](arg1: A1)(f: A1 => B, fname: String, body: String, substring: String)(implicit pr: Printer): B =
    printres(arg1)(x => printargs(x)(fname, y => printbody(y)(body, substring, f)))

  def reduceWhitespace(str: String): String = {
    val ws = " \n\r\t"
    str.foldLeft(""){
      (str, ch) =>
        if(str.size > 0 && ws.contains(str.last) && ws.contains(ch))
          str
        else
          str + (if(ws.contains(ch)) " " else ch)
    }
  }

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
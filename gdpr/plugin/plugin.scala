package nl.vindh.gdpr

import scala.tools.nsc.Global
import scala.tools.nsc.Phase
import scala.tools.nsc.plugins._
import scala.tools.nsc.transform._
class CompilerPlugin(override val global: Global)
  extends Plugin {
  override val name = "compiler-plugin"
  override val description = "Compiler plugin"
  override val components =
    List(new CompilerPluginComponent(global))
}
class CompilerPluginComponent(val global: Global)
  extends PluginComponent with TypingTransformers {
  import global._
  override val phaseName = "compiler-plugin-phase"
  override val runsAfter = List("typer")
  override def newPhase(prev: Phase) =
    new StdPhase(prev) {
      override def apply(unit: CompilationUnit) {
        unit.body = new MyTypingTransformer(unit).transform(unit.body)
      }
    }
  class MyTypingTransformer(unit: CompilationUnit)
    extends TypingTransformer(unit) {
    override def transform(tree: Tree) = tree match {
      case Typed(Apply(a, b), tpt) => {
        println("typedapply")
        println(a.getClass.getName)
        //println(a.qualifier.getClass.getDeclaredFields.map(_.getName).mkString(","))
        super.transform(tree)
      }
      case Apply(a, b) => {
        println("apply")
        super.transform(tree)
      }
      case _ => //println(showRaw(tree))
      super.transform(tree)
    }
  }
  def newTransformer(unit: CompilationUnit) =
    new MyTypingTransformer(unit)
}
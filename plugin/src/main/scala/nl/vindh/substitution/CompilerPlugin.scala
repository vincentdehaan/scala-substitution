package nl.vindh.substitution

import scala.tools.nsc
import nsc.{Global, Phase}
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent

class CompilerPlugin(val global: Global) extends Plugin {
  import global._

  val name = "substitution"
  val description = "substitutes some things"
  val components = List[PluginComponent](Component)

  private object Component extends PluginComponent {
    val global: CompilerPlugin.this.global.type = CompilerPlugin.this.global
    override val runsAfter: List[String] = List[String]("typer")
    override val phaseName: String = CompilerPlugin.this.name

    override def newPhase(prev: Phase): Phase = ???
    class SubstitutionPhase(_prev: Phase) extends StdPhase(_prev) {
      override def name = CompilerPlugin.this.name
      def apply(unit: CompilationUnit): Unit = {
        unit.body match {
          case Annotated(annot, arg) => println(annot)
        }
      }

    }
  }
}

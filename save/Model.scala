package co.blocke.scalabars

import co.blocke.scalajack.{ ScalaJack, json4s }
import org.json4s._
import scala.reflect.runtime.universe.TypeTag

object SB {
  type Scope = JValue

  implicit class OpsNum(val str: String) extends AnyVal {
    def isNumeric() = scala.util.Try(str.toDouble).isSuccess
  }

  def renderChunk(context: Context, options: Map[String, Any], renderables: List[Renderable])(implicit sb: Scalabars) =
    renderables.map(_.render(context, options)).mkString("")

  def swallowNothing(v: JValue) =
    v match {
      case JNothing => JString("")
      case _        => v
    }

  val sj = ScalaJack(json4s.Json4sFlavor())

}

object Options extends Enumeration {
  type Options = Value

  val data, compat, knownHelpers, knownHelpersOnly, noEscape, strict, assumeObjects, preventIntent, ignoreStandalone, explicitPartialContext = Value
}

trait Renderable {
  val label: String
  def render(context: Context, options: Map[String, Any])(implicit sb: Scalabars): String
}

trait Argument
//case class ObjectArgument(value: String) extends Argument
case class StringArgument(value: String) extends Argument
case class PathArgument(path: List[String]) extends Argument
case class AssignmentArgument(label: String, value: Argument) extends Argument

case class Template(compiled: List[Renderable])(implicit sb: Scalabars, options: Map[String, Any]) {
  def render[T](context: T)(implicit tt: TypeTag[T]): String = render(SB.sj.render(context))
  def render(context: SB.Scope): String = compiled.map(_.render(Context(context), options)).mkString("")
}
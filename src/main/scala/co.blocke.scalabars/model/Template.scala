package co.blocke.scalabars
package model

import scala.reflect.runtime.universe.TypeTag
import org.json4s._

trait Template {
  val compiled: List[Renderable]
  val compileOptions: Options

  def apply(dataObj: JValue): String =
    render(Context.root(dataObj).copy(partials = compileOptions.context.partials))
  def apply[T](dataObj: T)(implicit tt: TypeTag[T]): String = render(contextFromObj(dataObj))

  def render(context: Context): String

  def contextFromObj[T](obj: T)(implicit tt: TypeTag[T]): Context =
    Context.root(toJson4s(obj)).copy(partials = compileOptions.context.partials)

  override def toString: String = {
    val tabbed = compiled
      .map(_.toString)
      .map(s => s.split("\n").map(t => "     " + t).mkString("\n"))
      .mkString("\n")
    "Template:\n" + tabbed + "\n"
  }
}

case class EmptyTemplate() extends Template {
  val compileOptions: Options = null // Never used
  val compiled = List.empty[Renderable]
  def render(context: Context): String = ""
}

case class SBTemplate(
    compiled:       List[Renderable],
    compileOptions: Options
) extends Template {

  def render(context: Context): String = {
    val startingOpts =
      // Need to clear out _fn and _template in case this is a nested template render... If it is, then this template's
      // render will arrive here (incorrectly) pre-populated with _fn/_inverse from the parent template, which isn't what we want.
      compileOptions.copy(context  = context, _fn = EmptyTemplate(), _inverse = EmptyTemplate())
    val rc = compiled.foldLeft(RenderControl(startingOpts)) {
      case (rcX, renderable) =>
        renderable.render(rcX)
    }
    rc.out.toString + rc.accumulatedWS
  }
}

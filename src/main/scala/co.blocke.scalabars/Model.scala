package co.blocke.scalabars

import co.blocke.scalajack.{ ScalaJack, json4s }
import org.apache.commons.text.StringEscapeUtils
import org.json4s._

object SB {
  type Scope = JValue

  def renderChunk(context: Context, renderables: List[Renderable])(implicit sb: ScalaBars) =
    renderables.map(_.render(context)).mkString("")

  val sj = ScalaJack(json4s.Json4sFlavor())
  val pathParser = PathParser()
}
import SB._

object Context {
  def apply(value: Scope): Context = Context(value, List(value)) // history initially == scope at top-level
}
case class Context(value: Scope, history: List[Scope]) {
  def find(path: String): Context = {
    val newHistory = pathParser.unpackPath(path).foldLeft(history) {
      case (h, element) => element match {
        case ".." => h.tail
        case n if n.startsWith("[") => h.head match {
          case ja: JArray => ja.arr(n.tail.toInt) +: h
          case _          => throw new Exception("Boom -- expected an JArray here")
        }
        case e => h.head match {
          case jo: JObject => jo \ e +: h
          case _           => throw new Exception("Boom -- Expected JObject here!")
        }
      }
    }
    Context(newHistory.head, newHistory)
  }
}

trait Renderable {
  def render(context: Context)(implicit sb: ScalaBars): String
}

case class Comment() extends Renderable {
  def render(context: Context)(implicit sb: ScalaBars): String = ""
}

case class Text(value: String) extends Renderable {
  def render(context: Context)(implicit sb: ScalaBars) = value
}

case class Variable(name: String, escaped: Boolean) extends Renderable {
  def render(context: Context)(implicit sb: ScalaBars) =
    if (escaped)
      StringEscapeUtils.escapeHtml4(context.find(name).value.values.toString)
    else
      context.find(name).value.values.toString
}

case class Inverted(name: String, contained: List[Renderable]) extends Renderable {
  def render(context: Context)(implicit sb: ScalaBars) =
    context.find(name).value match {
      case JNothing                     => renderChunk(context, contained)
      case b: JBool if !b.value         => renderChunk(context, contained)
      case t: JArray if (t.arr.isEmpty) => renderChunk(context, contained)
      case _                            => ""
    }
}

case class Section(name: String, contained: List[Renderable]) extends Renderable {
  def render(context: Context)(implicit sb: ScalaBars) = {
    val resolved = context.find(name)
    resolved.value match {
      case b: JBool if b.value => renderChunk(context, contained)
      case c: JObject          => renderChunk(resolved, contained)
      case t: JArray => t.arr.foldLeft("") {
        case (str, jv) =>
          val ctx = Context(jv, jv +: resolved.history)
          str + renderChunk(ctx, contained)
      }
      case _ => ""
    }
  }
}

case class Partial(name: String, isDynamic: Boolean) extends Renderable {
  def render(context: Context)(implicit sb: ScalaBars) =
    if (isDynamic)
      sb.run(name)
    else
      sb.getPartial(name).map(contained => renderChunk(context, contained) + "\n").getOrElse(s"x$name x")
}
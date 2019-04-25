package co.blocke.scalabars

import org.apache.commons.text.StringEscapeUtils
import org.json4s._

object HB {
  type Scope = JValue

  def renderChunk(context: Context, renderables: List[Renderable]) =
    renderables.map(_.render(context)).mkString("")

  val pathParser = PathParser()
}
import HB._

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
  def render(context: Context): String
}

case class Text(value: String) extends Renderable {
  def render(context: Context) = value
}

case class Variable(name: String, escaped: Boolean) extends Renderable {
  def render(context: Context) =
    if (escaped)
      StringEscapeUtils.escapeHtml4(context.find(name).value.values.toString)
    else
      context.find(name).value.values.toString
}

case class Inverted(name: String, contained: List[Renderable]) extends Renderable {
  def render(context: Context) =
    context.find(name).value match {
      case JNothing                     => renderChunk(context, contained)
      case b: JBool if !b.value         => renderChunk(context, contained)
      case t: JArray if (t.arr.isEmpty) => renderChunk(context, contained)
      case _                            => ""
    }
}

case class Section(name: String, contained: List[Renderable]) extends Renderable {
  def render(context: Context) = {
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
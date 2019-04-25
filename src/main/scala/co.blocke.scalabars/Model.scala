package co.blocke.scalabars

import org.apache.commons.text.StringEscapeUtils
import org.json4s._

object HB {
  type Context = JObject

  def renderChunk(context: Context, renderables: List[Renderable]) =
    renderables.map(_.render(context)).mkString("")
}
import HB._

trait Renderable {
  def render(context: Context): String
}

case class Text(value: String) extends Renderable {
  def render(context: Context) = value
}

case class Variable(name: String, escaped: Boolean) extends Renderable {
  def render(context: Context) =
    if (escaped)
      StringEscapeUtils.escapeHtml4((context \ name).values.toString)
    else
      (context \ name).values.toString
}

case class Inverted(name: String, contained: List[Renderable]) extends Renderable {
  def render(context: Context) =
    context \ name match {
      case JNothing                     => renderChunk(context, contained)
      case b: JBool if !b.value         => renderChunk(context, contained)
      case t: JArray if (t.arr.isEmpty) => renderChunk(context, contained)
      case _                            => ""
    }
}

case class Section(name: String, contained: List[Renderable]) extends Renderable {
  def render(context: Context) =
    context \ name match {
      case b: JBool if b.value => renderChunk(context, contained)
      case c: JObject          => renderChunk(c, contained)
      case t: JArray           => t.arr.foldLeft("") { case (str, jv) => str + renderChunk(jv.asInstanceOf[Context], contained) }
      case _                   => ""
    }
}
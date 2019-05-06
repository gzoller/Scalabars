package co.blocke.scalabars

import SB._
import org.json4s._
import jdk.nashorn.api.scripting.JSObject

case class BlockRunner(context: Context, options: Map[String, Any], contents: List[Renderable])(implicit sb: Scalabars) {
  def fn(a: JSObject): String = {
    val newScope = ScriptToJson4s.toJson4s(a)
    renderChunk(Context(newScope, newScope +: context.history.tail), options, contents)
  }
}

case class Block(expr: Expr, contents: List[Renderable]) extends Renderable {
  def render(context: Context, options: Map[String, Any])(implicit sb: Scalabars): String = {

    sb.javascript.put("__options", BlockRunner(context, options, contents)) // gymnastics to pass an object to options.fn()

    expr match {
      case s: SimpleExpr if sb.helpers.contains(s.label) =>
        sb.run(FullExpr(s.label, List(ObjectArgument("__options"))), context, options)
      case s: SimpleExpr =>
        context.find(s.path, options).value match {
          case JNothing                     => ""
          case b: JBool if !b.value => ""
          case _                            => renderChunk(context, options, contents)
        }
      //      case f: FullExpr                                       => renderChunk(context, options, sb.parseMe(sb.run(f, context, options, None)))
    }
  }
}
package co.blocke.scalabars

import co.blocke.scalabars.SB.renderChunk
import org.apache.commons.text.StringEscapeUtils
import Options._
import SB._

// A thing can be:
// 1) A variable (unescaped)
// 2) A variable (escaped)
// 3) A helper function (with or w/o arguments)
case class Thing(expr: Expr, isEscaped: Boolean) extends Renderable {
  def render(context: Context, options: Map[String, Any])(implicit sb: Scalabars): String = {
    val stage1 = expr match {
      case s: SimpleExpr if sb.helpers.contains(s.path.head) => renderChunk(context, options, sb.parseMe(sb.run(FullExpr(s.path.head, List.empty[Argument]), context, options)))
      case s: SimpleExpr                                     => swallowNothing(context.find(s.path, options).value).values.toString
      case f: FullExpr                                       => renderChunk(context, options, sb.parseMe(sb.run(f, context, options)))
    }
    if (isEscaped && !options.get(noEscape.toString).asInstanceOf[Option[Boolean]].getOrElse(false) && !sb.eval("this.safestring").asInstanceOf[Boolean])
      StringEscapeUtils.escapeHtml4(stage1)
    else
      stage1
  }
}

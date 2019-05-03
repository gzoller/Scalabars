package co.blocke.scalabars

import co.blocke.scalabars.SB.renderChunk
import org.apache.commons.text.StringEscapeUtils

// A thing can be:
// 1) A variable (unescaped)
// 2) A variable (escaped)
// 3) A helper function (with or w/o arguments)
case class Thing(expr: Expr, isEscaped: Boolean) extends Renderable {
  def render(context: Context)(implicit sb: Scalabars) = {
    val stage1 = expr match {
      case s: SimpleExpr if sb.helpers.contains(s.path.head) => renderChunk(context, sb.parseMe(sb.run2(FullExpr(s.path.head, List.empty[Argument]), context, None)))
      case s: SimpleExpr                                     => context.find(s.path).value.values.toString
      case f: FullExpr                                       => renderChunk(context, sb.parseMe(sb.run2(f, context, None)))
    }
    println("Stage 1: " + stage1)
    if (isEscaped && sb.eval("this.safestring") == false)
      StringEscapeUtils.escapeHtml4(stage1)
    else
      stage1
  }
}

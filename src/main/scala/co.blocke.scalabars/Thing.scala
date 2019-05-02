package co.blocke.scalabars

import co.blocke.scalabars.SB.renderChunk
import org.apache.commons.text.StringEscapeUtils

// A thing can be:
// 1) A variable (unescaped)
// 2) A variable (escaped)
// 3) A helper function (with or w/o arguments)
case class Thing(expr: Expr, isEscaped: Boolean) extends Renderable {
  def render(context: Context)(implicit sb: ScalaBars) = expr match {
    case s: SimpleExpr if sb.helpers.contains(s.path.head) => renderChunk(context, sb.parseMe(sb.run2(FullExpr(s.path.head, List.empty[Argument]), context, None)))
    case s: SimpleExpr if isEscaped                        => StringEscapeUtils.escapeHtml4(context.find(s.path).value.values.toString)
    case s: SimpleExpr                                     => context.find(s.path).value.values.toString
    case f: FullExpr                                       => renderChunk(context, sb.parseMe(sb.run2(f, context, None)))
  }
}

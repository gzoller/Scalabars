package co.blocke.scalabars

import org.apache.commons.text.StringEscapeUtils
import SB._
import Options._

trait Expression extends Renderable {
  val path: List[String]
  val args: List[Argument]

  // TODO: Must eval simple path args + right-hand-side assignment path args
}

case class SimpleExpression(path: Path, args: List[Argument], isEscaped: Boolean = false) extends Expression {
  val label: String = path.last
  def render(context: Context, options: Map[String, Any])(implicit sb: Scalabars): String = {
    (sb, path) match {
      case IsHelper(helper) => helper.eval(context, this)
      case _ =>
        swallowNothing(context.find(path, options).value).values.toString match {
          case s if isEscaped && !options.get(noEscape.toString).asInstanceOf[Option[Boolean]].getOrElse(false) =>
            StringEscapeUtils.escapeHtml4(s)
          case s => s
        }
    }
  }
}

case class BlockExpression(path: List[String], args: List[Argument], contents: List[Renderable], isInverted: Boolean = false) extends Expression {
  val label: String = path.last
  def render(context: Context, options: Map[String, Any])(implicit sb: Scalabars): String = ""
}

case class Comment() extends Renderable {
  val label: String = "comment"
  def render(context: Context, options: Map[String, Any])(implicit sb: Scalabars): String = ""
}

case class Text(value: String) extends Renderable {
  val label: String = "text"
  def render(context: Context, options: Map[String, Any])(implicit sb: Scalabars): String = value
}

// Extractors

object IsHelper {
  def unapply(p: (Scalabars, List[String])): Option[Helper] = p._1.getHelper(p._2.head)
}

package co.blocke.scalabars

import org.apache.commons.text.StringEscapeUtils

trait Renderable {
  val label: String
  def render(options: Options): String
}

trait Expression extends Renderable {
  val path: Path
  val args: List[Argument]
}

// Can be:
// 1) a simple tag (replacement variable)
// 2) a no-arg helper label
// 3) a helper label + args
case class SimpleExpression(path: Path, args: List[Argument], isEscaped: Boolean = false) extends Expression {
  val label: String = path.last
  def render(options: Options): String = {
    val str: StringWrapper = (options.handlebars, path) match {
      case IsHelper(helper) => helper.eval(this, options)
      case _                => options.context.get.resolve(path, options)
    }
    str match {
      case _: RawStringWrapper if isEscaped && !options.hash("noEscape").asInstanceOf[Boolean] =>
        StringEscapeUtils.escapeHtml4(str.s)
      case _ =>
        str.s
    }
  }
}

case class BlockExpression(path: Path, args: List[Argument], contents: List[Renderable], isInverted: Boolean = false) extends Expression {
  val label: String = path.last
  def render(options: Options): String = ""
}

case class Comment() extends Renderable {
  val label: String = "comment"
  def render(options: Options): String = ""
}

case class Text(value: String) extends Renderable {
  val label: String = "text"
  def render(options: Options): String = value
}

// Extractors

object IsHelper {
  def unapply(p: (Scalabars, List[String])): Option[Helper] = p._1.helpers.get(p._2.head)
}

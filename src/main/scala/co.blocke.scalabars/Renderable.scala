package co.blocke.scalabars

import org.apache.commons.text.StringEscapeUtils
import org.json4s._

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
case class SimpleExpression(label: String, path: Path, args: List[Argument], isEscaped: Boolean = false) extends Expression {
  def render(options: Options): String = {
    val str: StringWrapper = (options.handlebars, path) match {
      case IsHelper(helper) => helper.eval(this, options)
      case _                => options.context.get.resolve(path, options)
    }
    str match {
      case _: RawStringWrapper if isEscaped && options.hash("noEscape") == "false" =>
        StringEscapeUtils.escapeHtml4(str.s)
      case _ =>
        str.s
    }
  }
}

// Can be:
// 1) Boolean (truthy/falsey) on a field
// 2) No-arg helper
// 3) Helper label + args
case class BlockExpression(label: String, path: Path, args: List[Argument], contents: List[Renderable], isInverted: Boolean = false) extends Expression {

  def render(options: Options): String = {
    (options.handlebars, path) match {
      case IsHelper(helper) =>
        val (fn, inverse) = examineBlock(options: Options)
        helper.eval(this, options.copy(helperName = path.head, _fn = Some(fn), _inverse = Some(inverse))).s
      case _ =>
        val cond = !options.isFalsy(options.context.get.find(path))
        if (cond || isInverted) {
          SimpleTemplate(contents, options).render(options.context.get)
        } else
          ""
    }
  }

  private def examineBlock(options: Options): (Template, Template) =
    contents.zipWithIndex.collectFirst {
      case (SimpleExpression("else", List("else"), _, _), i) => contents.splitAt(i)
    }.orElse(Some(contents, List.empty[Renderable])).map {
      case (fn, inv) => (SimpleTemplate(fn, options), SimpleTemplate(inv, options))
    }.get
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
  def unapply(p: (Scalabars, List[String])): Option[Helper] = p._1.getHelper(p._2.head)
}

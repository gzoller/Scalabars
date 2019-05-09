package co.blocke.scalabars

import org.json4s._
import org.apache.commons.text.StringEscapeUtils

abstract class Helper(val params: List[String] = List.empty[String]) {

  object Handlebars {
    def SafeString(s: String) = SafeStringWrapper(s)
    def escapeExpression(s: String) = StringEscapeUtils.escapeHtml4(s)
  }

  def eval(expr: Expression, options: Options): StringWrapper = {
    val (assignments, literalsAndPaths) = expr.args.partition(_.isInstanceOf[AssignmentArgument])

    // Poke assignments into "this" (Options.context, which must have value == JObject)
    val hashAdds = assignments.asInstanceOf[List[AssignmentArgument]].map(a => a.value match {
      case s: StringArgument => a.label -> JString(s.value)
      case p: PathArgument   => a.label -> options.context.get.find(p.path).value
    })
    val newContext = hashAdds.foldLeft(options.context.get) {
      case (ctx, kvpair) =>
        val (k, v) = kvpair
        val newHead = ctx.value.merge(JObject(k -> v))
        Context(newHead, newHead +: ctx.history.tail)
    }

    // Assign params to Options
    val newOptions = options.copy(params  = literalsAndPaths, context = Some(newContext))

    run(expr)(newOptions)
  }

  def resolve(target: String)(implicit options: Options): String = lookup(target).resolve(List("."), options)

  /*
    options.handlebars.pathCompile(target) match {
      case p if p.size == 1 => // either a parameter, or a hash key, or a this deref, in that order
        params.indexOf(p.head) match {
          case i if i < 0 => // look in hash, then 'this'
            options.hash.get(p.head).asInstanceOf[Option[Context]]
              .map(_.resolve(List("."), options))
              .orElse(Some(options.context.get.resolve(p, options))).get
          case i => // get Options.params[i] (passed-in helper parameters) and resolve
            options.params(i) match {
              case s: StringArgument => s.value
              case a: PathArgument   => options.context.get.resolve(a.path, options)
            }
        }
      case p if params.contains(p.head) =>
        options.params(params.indexOf(p.head)) match {
          case s: StringArgument => throw new BarsException("Can't dereference below terminal path element: " + s.value)
          case a: PathArgument   => options.context.get.find(a.path).resolve(p.tail, options)
        }
      case p => options.context.get.resolve(p, options)
    }
    */

  def options(implicit options: Options): Options = options

  def lookup(p: String)(implicit options: Options): Context = lookup(options.handlebars.pathCompile(p))
  def lookup(p: Path)(implicit options: Options): Context =
    p match {
      case p if p.size == 1 => // either a parameter, or a hash key, or a this deref, in that order
        params.indexOf(p.head) match {
          case i if i < 0 => // look in hash, then 'this'
            options.hash.get(p.head).asInstanceOf[Option[Context]]
              .orElse(Some(options.context.get.find(p))).get
          case i => // get Options.params[i] (passed-in helper parameters) and resolve
            options.params(i) match {
              case s: StringArgument => Context(JString(s.value))
              case a: PathArgument   => options.context.get.find(a.path)
            }
        }
      case p if params.contains(p.head) =>
        options.params(params.indexOf(p.head)) match {
          case s: StringArgument => throw new BarsException("Can't dereference below terminal path element: " + s.value)
          case a: PathArgument   => options.context.get.find(a.path ++ p.tail)
        }
      case p =>
        options.context.get.find(p)
    }

  def run(expr: Expression)(implicit options: Options): StringWrapper

}

trait StringWrapper { val s: String }
case class SafeStringWrapper(s: String) extends StringWrapper
case class RawStringWrapper(s: String) extends StringWrapper

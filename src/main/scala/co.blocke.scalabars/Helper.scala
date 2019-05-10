package co.blocke.scalabars

import org.json4s._
import org.apache.commons.text.StringEscapeUtils
import collection.JavaConverters._

object Handlebars {
  def SafeString(s: String) = SafeStringWrapper(s)
  def escapeExpression(s: String) = StringEscapeUtils.escapeHtml4(s)
}

abstract class Helper(val params: List[String] = List.empty[String]) {

  def eval(expr: Expression, options: Options): StringWrapper = {
    val (assignments, literalsAndPaths) = expr.args.partition(_.isInstanceOf[AssignmentArgument])

    // Poke assignments into Options.hash
    val hashAdds = assignments.asInstanceOf[List[AssignmentArgument]].map(a => a.label -> a.value).toMap

    // Assign params to Options
    val newOptions = options.copy(params = literalsAndPaths, _hash = options._hash ++ hashAdds)

    run(expr)(newOptions)
  }

  def resolve(target: String)(implicit options: Options): String = lookup(target).resolve(List("."), options)

  def options(implicit options: Options): Options = options

  def lookup(p: String)(implicit options: Options): Context = lookup(options.handlebars.pathCompile(p))

  def lookup(a: Argument)(implicit options: Options): Context = a match {
    case s: StringArgument => Context(JString(s.value))
    case p: PathArgument   => lookup(p.path)
  }

  def lookup(p: Path)(implicit options: Options): Context =
    p match {
      case p if p.size == 1 => // either a parameter, or a hash key, or a this deref, in that order
        params.indexOf(p.head) match {
          case i if i < 0 => // look in hash, then 'this'
            options.context.get.find(p)
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

  protected def packValue4js(v: JValue) = v match {
    case o: JObject => convert(o.values.asJava)
    case a: JArray  => convert(a.values) //a.values.map(x => x.asInstanceOf[Map[String, Any]].asJava).asJava
    case i          => i.values.toString
  }

  // Deep-convert Scala colletion to Java
  private def convert(x: Any): Object =
    {
      x match {
        case x: List[_]                        => x.map { convert }.asJava
        case x: collection.mutable.Map[_, _]   => x.mapValues(convert).asJava
        case x: collection.immutable.Map[_, _] => x.mapValues(convert).asJava
        case x: collection.Map[_, _]           => x.mapValues(convert).asJava
        case x: collection.mutable.Set[_]      => x.map(convert).asJava
        case x: collection.mutable.Buffer[_]   => x.map(convert).asJava
        case x: Iterable[_]                    => x.map(convert).asJava
        case x: Iterator[_]                    => x.map(convert).asJava
        case x: Array[_]                       => x.map(convert)
        case _                                 => x.asInstanceOf[Object]
      }
    }

  def run(expr: Expression)(implicit options: Options): StringWrapper

}

trait StringWrapper { val s: String }
case class SafeStringWrapper(s: String) extends StringWrapper
case class RawStringWrapper(s: String) extends StringWrapper

package co.blocke.scalabars
package model

import org.json4s._

abstract class Helper(argSymbols: String*) {

  def run()(implicit options: Options, partials: Map[String, Template]): EvalResult[_]

  def arg(symbol: String)(implicit options: Options): EvalResult[_] = {
    // This "arg" might be either 1) a simple arg symbol, or 2) a complex path with args in it, e.g. "x.[1]"
    // Handle both:.
    // NOTE: #2 must only contain 1 symbol at a time!  "x.[y]" is not allowed!
    // NOTE2: For #2, the symbol must be first in the path.  "/foo/bar/x/[1]" is not allowed (where x is the arg symbol)
    Context.pathParser.pathCompile(symbol) match {
      case p :: Nil => // #1
        argSymbols.indexOf(p) match {
          case i if i >= 0 => options.params.lift(i).getOrElse(NoEvalResult())
          case _           => NoEvalResult()
        }
      case p :: rest => // #2
        argSymbols.indexOf(p) match {
          case i if i >= 0 =>
            (options.params(i) >> StringEvalResult(rest.mkString("/"))).map(c => ContextEvalResult(c)).getOrElse(NoEvalResult())
          case _ =>
            NoEvalResult()
        }
    }
  }

  /**
   * Use this when you want to assert that resolve argument is a scalar.
   * @param symbol
   * @param options
   * @return Some(scalar value) or None if non-scalar
   */
  def scalarArg(symbol: String)(implicit options: Options): Option[Any] = arg(symbol) match {
    case c: ContextEvalResult =>
      c.value.value match {
        case _: JObject => None
        case _: JArray  => None
        case JNothing   => None
        case v: JValue  => Some(v.values)
      }
    case NoEvalResult() => None
    case e              => Some(e.value)
  }

  def contextArg(symbol: String)(implicit options: Options): Option[Context] = arg(symbol) match {
    case c: ContextEvalResult =>
      c.value.value match {
        case _: JObject => Some(c.value)
        case _: JArray  => Some(c.value)
        case _          => None
      }
    case _ => None
  }

  def rawArgValue(symbol: String)(implicit options: Options): String =
    argSymbols.indexOf(symbol) match {
      case i if i >= 0 => options.paramValues(i)
      case _           => throw new BarsException(s"unknown--symbol '$symbol' in helper code is unknown")
    }

  def lookup(basePath: String, incrementalPath: String)(implicit options: Options): Context =
    options.context.lookup(prefixPath(basePath, incrementalPath))

  def addBlockParam(inBP: Map[String, Context], param: String, value: Context): Map[String, Context] = inBP + (param -> value)
}

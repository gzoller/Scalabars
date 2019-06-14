package co.blocke.scalabars
package model

import org.json4s._
import parsing.PathParser

object Context {
  val NotFound = Context("", JNothing, List.empty[Context], Map.empty[String, EvalResult[_]], Map.empty[String, Template], Map.empty[String, Context])
  def root(v: JValue) = Context("/", v, List.empty[Context], Map.empty[String, EvalResult[_]], Map.empty[String, Template], Map.empty[String, Context])
  val pathParser = PathParser()
}
import Context._

case class Context(
    path:        String,
    value:       JValue,
    stack:       List[Context],
    data:        Map[String, EvalResult[_]],
    partials:    Map[String, Template],
    blockParams: Map[String, Context]) {

  def setData(key: String, value: EvalResult[_]): Context = this.copy(data = this.data + (key -> value))
  def setData(newAdds: Map[String, EvalResult[_]]): Context = this.copy(data = this.data ++ newAdds)

  def push(v: JValue, pathPart: String): Context = Context(bakePath(this, pathPart), v, this +: stack, this.data, this.partials, this.blockParams)

  private def bakePath(c: Context, part: String): String = c.path + { if (c.path.last != '/') "/" else "" } + part

  def lookup(unparsedPath: String): Context = {
    // Normalize leading '/' to @root/
    val path = pathParser.pathCompile(unparsedPath) match {
      // Some operations evaluate to a literal and push a synthetic value onto the stack, i.e. it's a value
      // that can't be arrived at by natigating down the original data tree.  These values have a path ending
      // with a '$' value.  When we do a lookup on such a path, we want to convert the $ value to '.', or
      // path relative to the synthetic thing we're starting with.
      case "" :: v :: rest if v.startsWith("$") => "." +: rest
      case "" :: rest                           => "@root" +: rest
      case p                                    => p
    }

    val newCtx = path.foldLeft(this) {
      case (ctx, pathElement) =>
        pathElement match {
          case "@root" => if (ctx.stack.isEmpty) ctx else ctx.stack.last
          case special if special.startsWith("@") && ctx.data.contains(special.tail) =>
            ctx.data(special.tail) match {
              case e: StringEvalResult     => ctx.push(JString(e.value), special)
              case e: SafeStringEvalResult => ctx.push(JString(e.value), special)
              case e: ContextEvalResult    => ctx.push(e.value.value, special)
              case e: LongEvalResult       => ctx.push(JLong(e.value), special)
              case e: DoubleEvalResult     => ctx.push(JDouble(e.value), special)
              case e: BooleanEvalResult    => ctx.push(JBool(e.value), special)
            }
          case "." | "this" => ctx
          case ".." =>
            if (ctx.stack.isEmpty)
              throw new BarsException("Path cannot back up (..) beyond history: " + unparsedPath)
            ctx.stack.head
          case num if num.startsWith("[")   => consumeNumberPart(unparsedPath, num.tail.dropRight(1).toInt, ctx)
          case num if num.isNumeric         => consumeNumberPart(unparsedPath, num.toInt, ctx)
          case e if blockParams.contains(e) => blockParams(e)
          case e => ctx.value match {
            case jo: JObject if jo.values.contains(e) => ctx.push(jo \ e, e)
            case jo: JObject                          => NotFound.copy(path = bakePath(ctx, e))
            case _                                    => throw new BarsException("Illegal attempt to reference a field on a non-object: " + unparsedPath)
          }
        }
    }
    newCtx.copy(stack = this +: this.stack)
  }

  /**
   * Sometimes you want multiple operations to appear as one in the stack, primarily so '..' resolution works.
   * (Remember that '..' in Handlebars is not a posix-style '..', but of a historical backtracking, so '..' refers
   * to last stack history visited, not "up 1 level" as you might expect.
   * @return
   */
  def flatten(): Context =
    if (stack.size > 1)
      this.copy(stack = stack.tail)
    else
      this

  private def consumeNumberPart(wholePath: String, index: Int, ctx: Context): Context = {
    ctx.value match {
      case a: JArray => ctx.push(a.arr(index), s"[$index]")
      // Handle bizzare "fake arrays" in Javascript JSON where the key is the int index in string form
      case o: JObject if (o \ index.toString != JNothing) => ctx.push(o \ index.toString, s"[$index]")
      case _ => throw new BarsException("Can't index into a non-array in path: " + wholePath)
    }
  }

  def toEvalResult(options: Options): EvalResult[_] =
    this.value match {
      case JNothing if options.hash("strict") == "true" => throw new BarsException("Path not found: " + path)
      case JNothing => NoEvalResult()
      case _: JObject => ContextEvalResult(this)
      case _: JArray => ContextEvalResult(this)
      case e: JBool => BooleanEvalResult(e.values)
      case _ => StringEvalResult(this.value.values.toString)
    }

  private def toStringPathValue(c: Context) = c.path + " --> " + c.value.toString.take(75) + "..."
  // We can't use toString because that's a valid/needed conversion for Context
  def show: String = {
    toStringPathValue(this) + "\n" +
      "   Stack:\n" + { if (stack.nonEmpty) stack.map(toStringPathValue).mkString("      ", "\n      ", "\n") else "" } +
      "   Data: " + data + "\n" +
      "   Partials: " + partials.keys.mkString(" ")
  }

  override def toString: String = {
    val s: String = value
    s
  }
}
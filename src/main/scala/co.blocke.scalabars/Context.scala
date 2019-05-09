package co.blocke.scalabars

import org.json4s._
import scala.util.Try

object Context {
  def apply(value: JValue): Context = Context(value, List(value)) // history initially == scope at top-level
}

case class Context(value: JValue, history: List[JValue]) {

  // Returns JNothing if path not found
  def find(path: Path): Context = {
    path match {
      case List("true")      => Context(JBool(true))
      case List("false")     => Context(JBool(false))
      case List("null")      => Context(JNull)
      case List("undefined") => Context(JNothing)
      case a if a.length == 1 && a.head.isNumeric() =>
        Context(Try(a.head.toLong).toOption.map(m => JLong(m)).getOrElse(JDouble(a.head.toDouble)))
      case _ =>
        val newHistory = path.foldLeft(history) {
          case (hist, pathElement) => pathElement match {
            case "." | "this" => hist
            case ".."         => hist.tail // pop up one level
            case n if n.startsWith("[") => hist.head match { // deref array index
              case ja: JArray => ja.arr(n.tail.dropRight(1).toInt) +: hist
              case _          => throw new BarsException(s"Illegal attempt to index (${n.tail.dropRight(1).toInt}) a non-array")
            }
            case e => hist.head match {
              case jo: JObject => jo \ e +: hist
              case _           => throw new BarsException(s"Illegal attempt to reference an object member ($e) on a non-object")
            }
          }
        }
        Context(newHistory.head, newHistory)
    }
  }

  def root: Context = Context(history.last)

  def resolve(path: Path, options: Options, swallowNothing: Boolean = true): String =
    find(path).value match {
      case JNothing if options.hash("strict") == "true" => throw new BarsException("Path not found: " + path.mkString("."))
      case JNothing => ""
      case j => j.values.toString
    }
}
